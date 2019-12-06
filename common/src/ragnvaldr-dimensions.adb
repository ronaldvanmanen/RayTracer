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

with Ragnvaldr.Numerics.Generic_Array_Operations;

use Ragnvaldr.Numerics.Generic_Array_Operations;

package body Ragnvaldr.Dimensions is

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

    function "*" (Left : Vector; Right : Length) return Displacement renames
      Instantiations."*";

    function "/" (Left : Displacement; Right : Length) return Vector renames
      Instantiations."/";

    function "+" (Left, Right : Displacement) return Displacement renames
      Instantiations."+";

    function "-" (Left, Right : Displacement) return Displacement renames
      Instantiations."-";

end Ragnvaldr.Dimensions;
