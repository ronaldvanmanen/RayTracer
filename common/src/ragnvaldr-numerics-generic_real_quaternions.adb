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

package body Ragnvaldr.Numerics.Generic_Real_Quaternions is

    function "*" (Left, Right : Quaternion) return Quaternion is
    begin
        return 
          (Re => Left.Re * Right.Re - Left.Im * Right.Im,
           Im => Left.Re * Right.Im + Right.Re * Left.Im + Left.Im * Right.Im);
    end "*";
      
    function "+" (Left, Right : Quaternion) return Quaternion is
    begin
        return (Re => Left.Re + Right.Re,
                Im => (Left.Im(1) + Right.Im(1),
                       Left.Im(2) + Right.Im(2),
                       Left.Im(3) + Right.Im(3)));
    end "+";
    
    function "-" (Left, Right : Quaternion) return Quaternion is
    begin
        return (Re => Left.Re - Right.Re,
                Im => (Left.Im(1) - Right.Im(1),
                       Left.Im(2) - Right.Im(2),
                       Left.Im(3) - Right.Im(3)));
    end "-";
    
    function Image (Value : Quaternion) return String is
    begin
        return 
          Real'Image (Value.Re) & "+"  &
          Real'Image (Value.Im (1)) & "i+" &
          Real'Image (Value.Im (2)) & "j+" &
          Real'Image (Value.Im (3)) & "k";
    end Image;

end Ragnvaldr.Numerics.Generic_Real_Quaternions;
