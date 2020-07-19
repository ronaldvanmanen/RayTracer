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

with Ragnvaldr.Dimensions;
with Ragnvaldr.Geometries;
with Ragnvaldr.Numerics.Generic_Real_Quaternions;

generic

    type Real is digits <>;
        
    with package Real_Dimensions is new Ragnvaldr.Dimensions(Real);

    with package Real_Geometries is new Ragnvaldr.Geometries(Real, Real_Dimensions);

    with package Real_Quaternions is new Ragnvaldr.Numerics.Generic_Real_Quaternions(Real);
        
package Ragnvaldr.Scene is

    use Real_Dimensions;
    use Real_Geometries;
    use Real_Quaternions;
        
    type Camera is private;

    function Make_Camera return Camera;

private

    type Camera is record
        Position : Point (1..3);
        Orientation : Quaternion;
        Frame_Width : Length;
        Frame_Height : Length;
        Frame_Aspect_Ratio : Scalar;
        Near_Clippling_Plane : Length;
        Far_Clipping_Plane : Length;
        Focal_Ratio : Scalar;
        Focal_Length : Length;
        Focal_Distance : Length;
    end record;
            
end Ragnvaldr.Scene;
