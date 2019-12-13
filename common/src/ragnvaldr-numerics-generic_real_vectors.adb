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

package body Ragnvaldr.Numerics.Generic_Real_Vectors is

    package Instantiations is

        function "*" is new
          Vector_Scalar_Elementwise_Operation
            (Left_Scalar   => Real,
             Right_Scalar  => Real,
             Result_Scalar => Real,
             Left_Vector   => Real_Vector,
             Result_Vector => Real_Vector,
             Operation     => "*");
        
        function "/" is new
          Vector_Scalar_Elementwise_Operation
            (Left_Scalar   => Real,
             Right_Scalar  => Real,
             Result_Scalar => Real,
             Left_Vector   => Real_Vector,
             Result_Vector => Real_Vector,
             Operation     => "/");
        
        function "+" is new
          Vector_Vector_Elementwise_Operation
            (Left_Scalar   => Real,
             Right_Scalar  => Real,
             Result_Scalar => Real,
             Left_Vector   => Real_Vector,
             Right_Vector  => Real_Vector,
             Result_Vector => Real_Vector,
             Operation     => "+");

        function "-" is new
          Vector_Vector_Elementwise_Operation
            (Left_Scalar   => Real,
             Right_Scalar  => Real,
             Result_Scalar => Real,
             Left_Vector   => Real_Vector,
             Right_Vector  => Real_Vector,
             Result_Vector => Real_Vector,
             Operation     => "-");

    end Instantiations;
    
    package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Real'Base);
    use Elementary_Functions;

    function "abs" (Value : Real_Vector) return Real is
    begin
        return Sqrt(Value * Value);
    end "abs";
      
    function "*" (Left, Right : Real_Vector) return Real is
    begin
        return Result : Real := 0.0 do
            for J in Left'Range loop
                Result := Result + Left (J) * Right (J - Left'First + Right'First);
            end loop;
        end return;
    end "*";

    function "+" (Left, Right : Real_Vector) return Real_Vector renames
      Instantiations."+";
              
    function "-" (Left, Right : Real_Vector) return Real_Vector renames
      Instantiations."-";

end Ragnvaldr.Numerics.Generic_Real_Vectors;
