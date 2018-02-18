# Lobo_Interpreter

**Authors: Christian Seely, John Clark**

Plumbing Graphics Code Written by Lance Williams.

**Example running program:**
runghc lobo_interp lissajous

**Description:**
Interpret and render Lobo code (modled after turtle graphics, see: https://en.wikipedia.org/wiki/Turtle_graphics) on the screen. 

**High Level:**
Lobo Code --> S-Expression --> Instruction Stream/ Jump Table --> Graphics Instruction Stream --> Rendered on Screen using Plumbing Graphics.  

**Lobo Code --> S-Expression**
The lobo code defined in a string is read in and converted to an S-Expression data type.

**S-Expression --> Instruction Stream/ Jump Table**
Next the S-Expression is parsed to produce a jump table and a custom instruction stream. The instructions in the instruction
stream are defined in our custom data type, Instruction.

**Instruction Stream/ Jump Table --> Graphics Instruction Stream**
Next using the instruction stream and jump table constructed in the previous step, a stream of basic graphics instructions is produced.
There are only four different graphics instructions produced during this phase of compilation:

Straight GLfloat  (Line of GLfloat in length)
Invisible GLfloat (Invisible line of GLfloat in length)
Bend GLfloat (Change the angle pen is facing)
Paint Colour Graphic (Change the color of pen)

All variable resolution is done at this stage in addition to function, arithmetic, and conditional evaluations.

**Graphics Instruction Stream --> Rendered on Screen using Plumbing Graphics**
Finally the graphics instruction stream is converted into a single recursive instance of the graphics data structure
using foldr1. After that the graphics instruction is send to Lance Williams rendering code and rendered to the screen.


**Examples:**














** The following license ONLY applies for code from the stages: 
S-Expression --> Instruction Stream/ Jump Table --> Graphics Instruction Stream**

    **Copyright (C) {2017}  {Christian Seely and John Clark} <cseely@unm.edu>**

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.