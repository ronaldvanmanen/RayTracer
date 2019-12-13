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

with Ragnvaldr.Dimensions; use Ragnvaldr.Dimensions;
with Ragnvaldr.Geometries; use Ragnvaldr.Geometries;
with Ragnvaldr.Numerics; use Ragnvaldr.Numerics;
with Ragnvaldr.Numerics.Real_Quaternions; use Ragnvaldr.Numerics.Real_Quaternions;

package Ragnvaldr.Raytracer is

    type Camera is private;

    function Make_Camera return Camera;

private
    
    type Camera is record
        Position : Point (1..3);
        Orientation : Quaternion;
        Frame_Width : Length;
        Frame_Height : Length;
        Frame_Aspect_Ratio : Float;
        Near_Clippling_Plane : Length;
        Far_Clipping_Plane : Length;
        Focal_Ratio : Float;
        Focal_Length : Length;
        Focal_Distance : Length;
    end record;
            
end Ragnvaldr.Raytracer;
