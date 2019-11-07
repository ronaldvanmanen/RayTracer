with Ada.Numerics.Real_Arrays;

use Ada.Numerics.Real_Arrays;

package Ragnvaldr.Raytracer is
  
    type Vector is new Real_Vector (1..3);

    type Ray is
    -- Represents a ray (of light).
       record
           Origin : Vector;
           -- The origin of the ray.
           Direction : Vector;
           -- The direction of the ray.
       end record;
   
    function Get_Point (A_Ray : Ray; A_Distance : Float) return Vector
    -- Returns the point at the specified distance from the origin of a ray.
    -- @param A_Ray The ray to interest.
    -- @param A_Distance The distance to travel along the ray.
    -- @return A vector with the coordinates of the point at the specified 
    -- distance along the ray.
      with Global => null, Pre => A_Distance > 0.0;
   
    type Sphere is
    -- Represents a sphere.
       record
           Center : Vector;
           -- The center of the sphere.
           Radius : Float;
           -- The radius of the sphere.
       end record;

    function Get_Surface_Normal(A_Sphere : Sphere; 
                                A_Point : Vector) return Vector
    -- Returns the surface normal at the specified intersection point
    -- @param A_Sphere The sphere to return the surface normal of.
    -- @param A_Point A point on the surface of the sphere.
    -- @return The surface normal.
      with Global => null;

    type Hit is 
    -- Represents a hit between a ray and an object.
       record
           Intersection_Point : Vector;
           -- The intersection point.
           Surface_Normal : Vector;
           -- The surface normal.
       end record;
   
    function Intersects (A_Ray : in Ray;
                         A_Sphere : in Sphere;
                         A_Hit : out Hit) return Boolean
      With Global => null;

end Ragnvaldr.Raytracer;
