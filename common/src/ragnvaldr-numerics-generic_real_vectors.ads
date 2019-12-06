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

generic
    
    type Real is digits <>;
    
package Ragnvaldr.Numerics.Generic_Real_Vectors is
       
    pragma Pure (Generic_Real_Vectors);

    type Real_Vector is array (Integer range<>) of Real;
    
    function "abs" (Value : Real_Vector) return Real
      with
        Global => null;

    function "*" (Left, Right : Real_Vector) return Real
      with
        Global => null,
        Pre => Left'Length = Right'Length;
            
    function "+" (Left, Right : Real_Vector) return Real_Vector
      with
        Global => null,
        Pre => Left'Length = Right'Length;
        
    function "-" (Left, Right : Real_Vector) return Real_Vector
      with
        Global => null,
        Pre => Left'Length = Right'Length,
        Post => "-"'Result'Length = Left'Length and "-"'Result'Length = Right'Length;
    
end Ragnvaldr.Numerics.Generic_Real_Vectors;
