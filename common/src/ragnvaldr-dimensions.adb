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

    generic
        type Right_Scalar is digits <>;
        type Sum_Scalar is digits<>;
        type Result_Scalar is digits <>;
        type Right_Vector is array (Integer range<>) of Right_Scalar;
    function Absolute_Value(Right : Right_Vector) return Result_Scalar;

    function Absolute_Value(Right : Right_Vector) return Result_Scalar is
        Sum : Sum_Scalar := 0.0;
    begin
        for J in Right'Range loop
            Sum := Sum + Sum_Scalar(Right (J) * Right (J));
        end loop;
        return Result_Scalar(Sqrt(Scalar(Sum)));
    end Absolute_Value;

    generic
        type Left_Scalar is digits <>;
        type Right_Scalar is digits <>;
        type Result_Scalar is digits <>;
        type Left_Vector is array (Integer range<>) of Left_Scalar;
        type Right_Vector is array (Integer range<>) of Right_Scalar;
    function Inner_Product (Left : Left_Vector; Right : Right_Vector) return Result_Scalar;

    function Inner_Product (Left : Left_Vector; Right : Right_Vector) return Result_Scalar is
    begin
        return Result : Result_Scalar := 0.0 do
            for J in Left'Range loop
                Result := Result + Result_Scalar(Scalar(Left (J)) * Scalar(Right (J - Left'First + Right'First)));
            end loop;
        end return;
    end Inner_Product;

    subtype Speed_Square is Scalar with Dimension =>
      (Symbol => "m/s", Meter => 2, Second => -2, others => 0);

    package Instantiations is

        function "abs" is new Absolute_Value
          (Right_Scalar => Scalar,
           Right_Vector => Vector,
           Sum_Scalar => Scalar,
           Result_Scalar => Scalar);

        function "abs" is new Absolute_Value
          (Right_Scalar => Length,
           Right_Vector => Displacement,
           Sum_Scalar => Area,
           Result_Scalar => Length);

        function "abs" is new Absolute_Value
          (Right_Scalar => Speed,
           Right_Vector => Velocity,
           Sum_Scalar => Speed_Square,
           Result_Scalar => Speed);

        function "*" is new Inner_Product
          (Left_Scalar => Length,
           Right_Scalar => Scalar,
           Result_Scalar => Length,
           Left_Vector => Displacement,
           Right_Vector => Vector);

        function "*" is new
          Vector_Scalar_Elementwise_Operation
            (Left_Scalar   => Length,
             Right_Scalar  => Length,
             Result_Scalar => Length,
             Left_Vector   => Vector,
             Result_Vector => Displacement,
             Operation     => "*");

        function "*" is new Inner_Product
          (Left_Scalar => Length,
           Right_Scalar => Length,
           Result_Scalar => Area,
           Left_Vector => Displacement,
           Right_Vector => Displacement);

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

    function "abs" (Right : Vector) return Scalar renames
      Instantiations."abs";

    function "abs" (Right : Displacement) return Length renames
      Instantiations."abs";

    function "abs" (Right : Velocity) return Speed renames
      Instantiations."abs";

    function "*" (Left : Vector; Right : Length) return Displacement renames
      Instantiations."*";

    function "*" (Left, Right : Displacement) return Area renames
      Instantiations."*";

    function "*" (Left : Displacement; Right : Vector) return Length renames
      Instantiations."*";

    function "/" (Left : Vector; Right : Scalar) return Vector renames
      Instantiations."/";

    function "/" (Left : Displacement; Right : Length) return Vector renames
      Instantiations."/";

    function "+" (Left, Right : Displacement) return Displacement renames
      Instantiations."+";

    function "-" (Left, Right : Displacement) return Displacement renames
      Instantiations."-";

end Ragnvaldr.Dimensions;
