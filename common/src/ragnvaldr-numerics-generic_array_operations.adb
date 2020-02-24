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

package body Ragnvaldr.Numerics.Generic_Array_Operations is

    function Absolute_Value (Right : Right_Vector) return Result_Scalar is
    begin
        return Sqrt(Right * Right);
    end Absolute_Value;

    function Vector_Scalar_Elementwise_Operation
      (Left  : Left_Vector;
       Right : Right_Scalar) return Result_Vector
    is
    begin
        return R : Result_Vector(Left'Range) do
            for J in Left'Range loop
                R (J) := Operation (Left (J), Right);
            end loop;
        end return;
    end Vector_Scalar_Elementwise_Operation;   

    function Scalar_Vector_Elementwise_Operation
      (Left  : Left_Scalar;
       Right : Right_Vector) return Result_Vector
    is
    begin
        return R : Result_Vector(Right'Range) do
            for J in Right'Range loop
                R (J) := Operation (Left, Right (J));
            end loop;
        end return;
    end Scalar_Vector_Elementwise_Operation;   

    function Vector_Vector_Elementwise_Operation
      (Left  : Left_Vector;
       Right : Right_Vector) return Result_Vector
    is
    begin
        return R : Result_Vector(Left'Range) do
            for J in Left'Range loop
                R (J) := Operation (Left (J), Right (J - Left'First + Right'First));
            end loop;
        end return;
    end Vector_Vector_Elementwise_Operation;   

    function Inner_Product
      (Left  : Left_Vector;
       Right : Right_Vector) return Result_Scalar
    is
    begin
        return Result : Result_Scalar := Zero do
            for J in Left'Range loop
                Result := Result + Left (J) * Right (J - Left'First + Right'First);
            end loop;
        end return;
    end Inner_Product;   

end Ragnvaldr.Numerics.Generic_Array_Operations;
