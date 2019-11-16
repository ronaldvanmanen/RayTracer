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

with Ada.Numerics.Real_Arrays;

use Ada.Numerics.Real_Arrays;

package Ragnvaldr.Raytracer is

    subtype Distance is Float range Float'Small .. Float'Large;
    
    type Vector is new Real_Vector (1..3);

    type EulerVector is record
        Axis : Vector;
        Angle : Float;
    end record;

    type Ray is record
        -- Represents a ray (of light).
        Origin : Vector;
        -- The origin of the ray.
        Direction : Vector;
        -- The direction of the ray.
    end record;
    
    function Make_Ray(Origin : Vector; Direction : Vector) return Ray;

    function Get_Point (A_Ray : Ray; A_Distance : Distance) return Vector
    --  Returns the point at the specified distance from the origin of a ray.
    --  @param A_Ray The ray to interest.
    --  @param A_Distance The distance to travel along the ray.
    --  @return A vector with the coordinates of the point at the specified 
    --  distance along the ray.
      with Global => null, Pre => A_Distance > 0.0;

    type Sphere is record
        -- Represents a sphere.
        Position : Vector;
        --  The center of the sphere.
        Radius : Distance;
        --  The radius of the sphere.
    end record;

    function Get_Surface_Normal(A_Sphere : Sphere; 
                                A_Point : Vector) return Vector
    --  Returns the surface normal at the specified intersection point
    --  @param A_Sphere The sphere to return the surface normal of.
    --  @param A_Point A point on the surface of the sphere.
    --  @return The surface normal.
      with 
        Global => null,
        Pre => abs(A_Point - A_Sphere.Position) = A_Sphere.Radius,
        Post => Get_Surface_Normal'Result = (A_Point - A_Sphere.Position) / A_Sphere.Radius;
            
    type Intersections is array (Positive range <>) of Distance;
    
    No_Intersections : constant Intersections (1..0) := (others => 0.0);
    
    function Intersects (A_Ray : in Ray;
                         A_Sphere : in Sphere) return Intersections
      with Global => null;
    
    type Camera is tagged private;
    
    function Make_Camera return Camera;
    
private
    type Camera is tagged record
        Position : Vector;
        Orientation : EulerVector;
    end record;
            
end Ragnvaldr.Raytracer;
