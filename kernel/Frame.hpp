// Frame.hpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#ifndef __FRAME_HPP
#define __FRAME_HPP

// #define ENABLE_CONTROL_FRAME_STATISTICS

class UnwindProtect;

enum FrameType
{
  PRIMORDIAL = 0,
  TAGBODY = 1,
  BLOCK = 2,
  CATCH = 3,
  UNWIND_PROTECT = 4
};

class Frame : public gc
{
private:
  FrameType _type;
  JMP_BUF _jmp;
  void * _last_special_binding;
  Frame * _last_control_frame;
  Tag * _last_tag;
  StackFrame * _stack;
  unsigned int _call_depth;
  Frame * _next;

  void init()
  {
    init(current_thread());
  }

protected:
  // UNWIND_PROTECT frames only
  Value _cleanup_forms;
  Environment * _env;
  void * _code;
#ifdef __x86_64__
  long _rbp;
#else
  int _ebp;
#endif

  // BLOCK frames only
  Value _block_name;

  // CATCH frames only
  Value _catch_tag;

public:
  void init(Thread * thread)
  {
    _last_special_binding = thread->last_special_binding();
    _last_control_frame = thread->last_control_frame();
    _last_tag = thread->last_tag();
    _stack = thread->stack();
    _call_depth = thread->call_depth();
  }

  Frame()
    : _type(PRIMORDIAL), _next(NULL)
  {
    init();
  }

  Frame(FrameType type)
    : _type(type)
  {
  }

  Frame(FrameType type, Frame * next)
    : _type(type), _next(next)
  {
    init();
  }

  Frame(FrameType type, Frame * next, Thread * thread)
    : _type(type), _next(next)
  {
    init(thread);
  }

  void clear()
  {
    memset(this, 0, sizeof(*this));
  }

  FrameType type()
  {
    return _type;
  }

  void set_type(FrameType type)
  {
    _type = type;
  }

  JMP_BUF * jmp()
  {
    return &(this->_jmp);
  }

  void * last_special_binding() const
  {
    return _last_special_binding;
  }

  Frame * last_control_frame() const
  {
    return _last_control_frame;
  }

  Tag * last_tag() const
  {
    return _last_tag;
  }

  StackFrame * stack() const
  {
    return _stack;
  }

  unsigned int call_depth() const
  {
    return _call_depth;
  }

  Frame * next() const
  {
    return _next;
  }

  void set_next(Frame * frame)
  {
    if (frame == this)
      {
        printf("Frame::set_next() frame == this\n");
        extern Value SYS_int3();
        SYS_int3();
      }
    _next = frame;
  }
};

class Tag : public gc
{
private:
  Value _name; // symbol or integer
  Tagbody * _tagbody;
  Value _continuation;
  int _index;
  Tag * _next;

public:
  Tag(Value name, Tagbody * tagbody, Value continuation, int index, Tag * next)
    : _name(name), _tagbody(tagbody), _continuation(continuation), _index(index), _next(next)
  {
  }

  Tag(Value name, Tagbody * tagbody, Value continuation, Tag * next)
    : _name(name), _tagbody(tagbody), _continuation(continuation), _index(0), _next(next)
  {
  }

  Tag(Value name, Tagbody * tagbody, int index, Tag * next)
    : _name(name), _tagbody(tagbody), _continuation(0), _index(index), _next(next)
  {
  }

  Value name() const
  {
    return _name;
  }

  Tagbody * tagbody() const
  {
    return _tagbody;
  }

  Value continuation() const
  {
    return _continuation;
  }

  int index () const
  {
    return _index;
  }

  Tag * next() const
  {
    return _next;
  }
};

class Tagbody : public Frame
{
public:
  Tagbody(Thread * thread)
    : Frame(TAGBODY, thread->last_control_frame(), thread)
  {
    thread->set_last_control_frame(this);
  }
};

class Block : public Frame
{
public:
  Block()
    : Frame(BLOCK)
  {
  }

  Value name() const
  {
    return _block_name;
  }

  void set_name(Value arg)
  {
    _block_name = arg;
  }
};

#define FRAME_POOL_SIZE 128

class FramePool : public gc
{
private:
  Frame * _pool[FRAME_POOL_SIZE];
  unsigned int _index;

  unsigned int _max_index;
  unsigned long _number_released;
  unsigned long _number_released_block;
  unsigned long _number_released_tagbody;
  unsigned long _number_released_unwind_protect;
  unsigned long _number_released_catch;
  unsigned long _number_ignored;
  unsigned long _number_reused;
  unsigned long _number_new;

public:
  FramePool()
    : _index(0)
  {
    memset(_pool, 0, FRAME_POOL_SIZE * sizeof(Frame *));
    _max_index = 0;
    _number_released = 0;
    _number_released_block = 0;
    _number_released_tagbody = 0;
    _number_released_unwind_protect = 0;
    _number_ignored = 0;
    _number_reused = 0;
    _number_new = 0;
  }

  void print_statistics()
  {
#ifdef ENABLE_CONTROL_FRAME_STATISTICS
    printf("Pool statistics:\n");
    printf("  released  = %lu (block = %lu, tagbody = %lu, unwind_protect = %lu, catch = %lu)\n",
           _number_released, _number_released_block, _number_released_tagbody,
           _number_released_unwind_protect, _number_released_catch);
    printf("  ignored   = %lu\n", _number_ignored);
    printf("  reused    = %lu\n", _number_reused);
    printf("  new       = %lu\n", _number_new);
    printf("  index     = %u\n", _index);
    printf("  max_index = %u\n", _max_index);
#endif
  }

  Frame * get_frame()
  {
#ifdef ENABLE_CONTROL_FRAME_STATISTICS
    if (_index > 0)
      _number_reused++;
    else
      _number_new++;
#endif
    return _index > 0 ? _pool[--_index] : NULL;
  }

  void release_frame(Frame * frame)
  {
    if (_index < FRAME_POOL_SIZE - 1)
      {
#ifdef ENABLE_CONTROL_FRAME_STATISTICS
        _number_released++;
        FrameType type = frame->type();
        if (type == TAGBODY)
          _number_released_tagbody++;
        else if (type == BLOCK)
          _number_released_block++;
        else if (type == UNWIND_PROTECT)
          _number_released_unwind_protect++;
        else if (type == CATCH)
          _number_released_catch++;
#endif
        frame->clear();
        _pool[_index++] = frame;
        if (_index > _max_index)
          _max_index = _index;
      }
#ifdef ENABLE_CONTROL_FRAME_STATISTICS
    else
      _number_ignored++;
#endif
  }
};

class Catch : public Frame
{
public:
  Catch(Value tag, Frame * next, Thread * thread)
    : Frame(CATCH, next, thread)
  {
    _catch_tag = tag;
  }

  Value tag() const
  {
    return _catch_tag;
  }

  void set_tag(Value tag)
  {
    _catch_tag = tag;
  }
};

#endif // Frame.hpp
