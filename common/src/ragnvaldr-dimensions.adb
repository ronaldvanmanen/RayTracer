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

with Ada.Numerics.Generic_Elementary_Functions;
with Ragnvaldr.Numerics.Generic_Array_Operations;
use Ragnvaldr.Numerics.Generic_Array_Operations;

package body Ragnvaldr.Dimensions is

    package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Scalar);
    use Elementary_Functions;

    package Instantiations is

        function "*" is new
          Vector_Scalar_Elementwise_Operation
            (Left_Scalar   => Length,
             Right_Scalar  => Length,
             Result_Scalar => Length,
             Left_Vector   => Vector,
             Result_Vector => Displacement,
             Operation     => "*");

        function "/" is new
          Vector_Scalar_Elementwise_Operation
            (Left_Scalar   => Scalar,
             Right_Scalar  => Scalar,
             Result_Scalar => Scalar,
             Left_Vector   => Vector,
             Result_Vector => Vector,
             Operation     => "/");

        function "/" is new
          Vector_Scalar_Elementwise_Operation
            (Left_Scalar   => Length,
             Right_Scalar  => Length,
             Result_Scalar => Scalar,
             Left_Vector   => Displacement,
             Result_Vector => Vector,
             Operation     => "/");

        function "+"  is new
          Vector_Vector_Elementwise_Operation
            (Left_Scalar   => Length,
             Right_Scalar  => Length,
             Result_Scalar => Length,
             Left_Vector   => Displacement,
             Right_Vector  => Displacement,
             Result_Vector => Displacement,
             Operation     => "+");

        function "-" is new
          Vector_Vector_Elementwise_Operation
            (Left_Scalar   => Length,
             Right_Scalar  => Length,
             Result_Scalar => Length,
             Left_Vector   => Displacement,
             Right_Vector  => Displacement,
             Result_Vector => Displacement,
             Operation     => "-");

    end Instantiations;

    function "abs" (Right : Vector) return Scalar is
    begin
        return Result : Scalar := 0.0 do
            for J in Right'Range loop
                Result := Result + Right (J) * Right (J);
            end loop;
            Result := Sqrt(Result);
        end return;
    end "abs";

    function "*" (Left : Vector; Right : Length) return Displacement renames
      Instantiations."*";

    function "*" (Left, Right : Displacement) return Area is
    begin
        return Result : Area := 0.0 do
            for J in Left'Range loop
                Result := Result + Left (J) * Right (J - Left'First + Right'First);
            end loop;
        end return;
    end "*";

    function "*" (Left : Displacement; Right : Vector) return Length is
    begin
        return Result : Length := 0.0 do
            for J in Left'Range loop
                Result := Result + Left (J) * Right (J - Left'First + Right'First);
            end loop;
        end return;
    end "*";

    function "/" (Left : Vector; Right : Scalar) return Vector renames
      Instantiations."/";

    function "/" (Left : Displacement; Right : Length) return Vector renames
      Instantiations."/";

    function "+" (Left, Right : Displacement) return Displacement renames
      Instantiations."+";

    function "-" (Left, Right : Displacement) return Displacement renames
      Instantiations."-";

end Ragnvaldr.Dimensions;
