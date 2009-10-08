// StackFramePool.hpp
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

#ifndef __STACK_FRAME_POOL_HPP
#define __STACK_FRAME_POOL_HPP

#include "StackFrame.hpp"

#define STACK_FRAME_POOL_SIZE 256

class StackFramePool : public gc
{
private:
  StackFrame * _pool[STACK_FRAME_POOL_SIZE];
  unsigned int index;

public:
  StackFramePool()
    : index(0)
  {
    memset(_pool, 0, STACK_FRAME_POOL_SIZE * sizeof(StackFrame *));
  }

  StackFrame * get_stack_frame()
  {
    return index > 0 ? _pool[--index] : NULL;
  }

  void release_stack_frame(StackFrame * frame)
  {
    if (index < STACK_FRAME_POOL_SIZE - 1)
      {
        frame->clear();
        _pool[index++] = frame;
      }
  }
};

#endif // StackFramePool.hpp
