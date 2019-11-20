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

    function "+" (Left, Right : Quaternion) return Quaternion is
    begin
        return (A => Left.A + Right.A, 
                B => Left.B + Right.B, 
                C => Left.C + Right.C, 
                D => Left.D + Right.D);
    end "+";
    
    function "-" (Left, Right : Quaternion) return Quaternion is
    begin
        return (A => Left.A - Right.A, 
                B => Left.B - Right.B, 
                C => Left.C - Right.C, 
                D => Left.D - Right.D);
    end "-";
    
    function Image (Value : Quaternion) return String is
    begin
        return 
          Real'Image (Value.A) & "+"  &
          Real'Image (Value.B) & "i+" &
          Real'Image (Value.C) & "j+" &
          Real'Image (Value.D) & "k";
    end Image;

end Ragnvaldr.Numerics.Generic_Real_Quaternions;
