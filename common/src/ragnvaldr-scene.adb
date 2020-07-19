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
with Ragnvaldr.Dimensions;
with Ragnvaldr.Geometries;

package body Ragnvaldr.Scene is

    Zero : Scalar := 0.0;
    
    Positive_Infinity : constant Scalar := 1.0 / Zero; 
    
    function Make_Camera return Camera is
    begin
        return Camera'
          (
           Position => (0.0 * Meter, 0.0 * Meter, 0.0 * Meter),
           Orientation => (Re => 0.0, Im => (0.0, 0.0, 0.0)),
           Frame_Width => 36.0 * Millimeter,
           Frame_Height => 24.0 * Millimeter,
           Frame_Aspect_Ratio => 4.0 / 3.0,
           Near_Clippling_Plane => Length'Epsilon,
           Far_Clipping_Plane => Length'Last,
           Focal_Ratio => Positive_Infinity,
           Focal_Length => 35.0 * Millimeter,
           Focal_Distance => Length'Last
          );
    end Make_Camera;
        
end Ragnvaldr.Scene;
