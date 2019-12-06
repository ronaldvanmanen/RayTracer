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

    function Vector_Scalar_Elementwise_Operation
      (Left  : Left_Vector;
       Right : Right_Scalar) return Result_Vector
    is
    begin
        return R : Result_Vector(Left'Range) do
            for J in R'Range loop
                R (J) := Operation (Left (J), Right);
            end loop;
        end return;
    end Vector_Scalar_Elementwise_Operation;   

    function Vector_Vector_Elementwise_Operation
      (Left  : Left_Vector;
       Right : Right_Vector) return Result_Vector
    is
    begin
        return R : Result_Vector(Left'Range) do
            for J in R'Range loop
                R (J) := Operation (Left (J), Right (J - R'First + Right'First));
            end loop;
        end return;
    end Vector_Vector_Elementwise_Operation;   

end Ragnvaldr.Numerics.Generic_Array_Operations;
