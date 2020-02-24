--  Ragnvaldr Systems -- Raytracer
--
--  Copyright (C) 2019  Ronald van Manen
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

package Ragnvaldr.Numerics.Generic_Array_Operations is

    pragma Pure (Generic_Array_Operations);
        
    generic
        type Right_Scalar is private;
        type Result_Scalar is private;
        type Right_Vector is array (Integer range<>) of Right_Scalar;
        Zero : Result_Scalar;
        with function Sqrt(Value : Result_Scalar) return Result_Scalar;
        with function "+"
          (Left  : Result_Scalar; 
           Right : Result_Scalar) return Result_Scalar;
        with function "*"
          (Left  : Right_Scalar; 
           Right : Right_Scalar) return Result_Scalar;
    function Absolute_Value (Right : Right_Vector) return Result_Scalar;
    
    generic
        type Left_Scalar is private;
        type Right_Scalar is private;
        type Result_Scalar is private;
        type Left_Vector is array (Integer range<>) of Left_Scalar;
        type Result_Vector is array (Integer range<>) of Result_Scalar;
        with function Operation
          (Left  : Left_Scalar;
           Right : Right_Scalar) return Result_Scalar;
    function Vector_Scalar_Elementwise_Operation
      (Left  : Left_Vector;
       Right : Right_Scalar) return Result_Vector;

    generic
        type Left_Scalar is private;
        type Right_Scalar is private;
        type Result_Scalar is private;
        type Right_Vector is array (Integer range<>) of Right_Scalar;
        type Result_Vector is array (Integer range<>) of Result_Scalar;
        with function Operation
          (Left  : Left_Scalar;
           Right : Right_Scalar) return Result_Scalar;
    function Scalar_Vector_Elementwise_Operation
      (Left  : Left_Scalar;
       Right : Right_Vector) return Result_Vector;

    generic
        type Left_Scalar is private;
        type Right_Scalar is private;
        type Result_Scalar is private;
        type Left_Vector is array (Integer range<>) of Left_Scalar;
        type Right_Vector is array (Integer range<>) of Right_Scalar;
        type Result_Vector is array (Integer range<>) of Result_Scalar;
        with function Operation
          (Left  : Left_Scalar;
           Right : Right_Scalar) return Result_Scalar;
    function Vector_Vector_Elementwise_Operation
      (Left  : Left_Vector;
       Right : Right_Vector) return Result_Vector;

    generic
        type Left_Scalar is private;
        type Right_Scalar is private;
        type Result_Scalar is private;
        type Left_Vector is array (Integer range<>) of Left_Scalar;
        type Right_Vector is array (Integer range<>) of Right_Scalar;
        Zero : Result_Scalar;
        with function "+"
          (Left  : Result_Scalar; 
           Right : Result_Scalar) return Result_Scalar;
        with function "*"
          (Left  : Left_Scalar; 
           Right : Right_Scalar) return Result_Scalar;
    function Inner_Product
      (Left  : Left_Vector;
       Right : Right_Vector) return Result_Scalar;

end Ragnvaldr.Numerics.Generic_Array_Operations;
