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

package body Ragnvaldr.Numerics.Generic_Real_Vectors is
    
    package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Real'Base);
    use Elementary_Functions;

    function "abs" (Value : Real_Vector) return Real is
    begin
        return Sqrt(Value * Value);
    end "abs";
      
    function "*" (Left, Right : Real_Vector) return Real is
        Result : Real := 0.0;
    begin
        for I in Left'Range loop
            Result := Result + Real'(Left (I) * Right (I));
        end loop;
        return Result;
    end "*";     

    function "+" (Left, Right : Real_Vector) return Real_Vector is
    begin
        return Result : Real_Vector do
            for I in Result'Range loop
                Result (I) := Left (I) + Right (I);
            end loop;
        end return;    
    end "+";
              
    function "-" (Left, Right : Real_Vector) return Real_Vector is
    begin
        return Result : Real_Vector do
            for I in Result'Range loop
                Result (I) := Left (I) - Right (I);
            end loop;
        end return;    
    end "-";

end Ragnvaldr.Numerics.Generic_Real_Vectors;
