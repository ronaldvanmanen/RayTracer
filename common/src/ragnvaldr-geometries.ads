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

package Ragnvaldr.Geometries is

    type Point is array(Integer range <>) of Length;
    
    function "+" (Left : Point; Right : Displacement) return Point
      with
        Global => null,
        Pre => Left'Length = Right'Length,
        Post => "+"'Result'Length = Left'Length;
    
    function "-" (Left, Right : Point) return Displacement
      with
        Global => null,
        Pre => Left'Length = Right'Length,
        Post => "-"'Result'Length = Left'Length;

    type Ray is private;

    function Make_Ray(Origin : Point; Direction : Vector) return Ray
      with Global => null;

    function Get_Point (A_Ray : Ray; A_Distance : Length) return Point
    --  Returns the point at the specified distance from the origin of a ray.
    --  @param A_Ray The ray to interest.
    --  @param A_Distance The distance to travel along the ray.
    --  @return A vector with the coordinates of the point at the specified 
    --  distance along the ray.
      with Global => null, Pre => A_Distance > 0.0;
    
    type Hit_State is (Entering, Exiting);

    type Hit is record
        Distance : Length;
        State : Hit_State;        
    end record;

    type Hit_Array is array (Positive range <>) of Hit;

    Empty_Hit_Array : constant Hit_Array (1..0) := 
      (others => Hit'(Distance => 0.0 * Meter, State => Entering));
    
    
    
    
    type Sphere is private;
    
    function Make_Sphere(Radius : Length) return Sphere;
    
    function Radius(A_Sphere : Sphere) return Length;
    
    function Surface_Area(A_Sphere : Sphere) return Area;

    function Surface_Normal(A_Sphere : Sphere; 
                            A_Point : Point) return Vector
    --  Returns the surface normal at the specified intersection point
    --  @param A_Sphere The sphere to return the surface normal of.
    --  @param A_Point A point on the surface of the sphere.
    --  @return The surface normal.
      with
        Global => null;
    
    function Enclosed_Volume(S : Sphere) return Volume;

    function Intersect (A_Ray : in Ray; A_Sphere : in Sphere) return Hit_Array
      with Global => null;

private

    type Ray is record
        Origin : Point (1..3);
        Direction : Vector (1..3);
    end record;

    type Sphere is record
        Center : Point (1..3);
        Radius : Length;
    end record;

end Ragnvaldr.Geometries;
