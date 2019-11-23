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

with Ragnvaldr.Numerics; use Ragnvaldr.Numerics;

package Ragnvaldr.Raytracer is

    type Ray is record
        -- Represents a ray (of light).
        Origin : Vector;
        -- The origin of the ray.
        Direction : Vector;
        -- The direction of the ray.
    end record;

    function Make_Ray(Origin : Vector; 
                      Direction : Vector) return Ray
      with Global => null;

    function Get_Point (A_Ray : Ray; 
                        A_Distance : Length) return Vector
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
        Radius : Length;
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
          
    type Hit_State is (Entering, Exiting);

    type Hit is record
        Distance : Length;
        State : Hit_State;        
    end record;

    type Hit_Array is array (Positive range <>) of Hit;

    Empty_Hit_Array : constant Hit_Array (1..0) := 
      (others => Hit'(Distance => 0.0, State => Entering));

    function Intersects (A_Ray : in Ray;
                         A_Sphere : in Sphere) return Hit_Array
      with Global => null;

    type Camera is tagged private;

    function Make_Camera return Camera;

private
    type Camera is tagged record
        Position : Vector;
        Orientation : Euler_Vector;
        Frame_Width : Integer;
        Frame_Height : Integer;
        Frame_Aspect_Ratio : Float;
        Near_Clippling_Plane : Length;
        Far_Clipping_Plane : Length;
        Focal_Ratio : Float;
        Focal_Length : Length;
        Focal_Distance : Length;
    end record;
            
end Ragnvaldr.Raytracer;
