with Ada.Numerics.Generic_Elementary_Functions;

package body Ragnvaldr.Raytracer is

    package Float_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);

    use Float_Elementary_Functions;

    function Get_Point (A_Ray : Ray;
                        A_Distance : Float) return Vector is
    begin -- Get_Point;
        return A_Ray.Origin + A_Ray.Direction * A_Distance;
    end Get_Point;

    function Get_Surface_Normal(A_Sphere : Sphere; 
                                A_Point : Vector) return Vector is
    begin -- Get_Surface_Normal
        return (A_Point - A_Sphere.Position) / A_Sphere.Radius;
    end Get_Surface_Normal;

    function Intersects (A_Ray : in Ray;
                         A_Sphere : in Sphere;
                         A_Hit : out Hit) return Boolean is
      
        Origin_To_Center : Vector;
        Length_Squared_Of_Origin_To_Center : Float;
        Radius_Of_Sphere_Squared : Float;
        Is_Outside : Boolean;
        Closest_Approach : Float;
        Points_Away : Boolean;
        Half_Chord_Distance_Squared : Float;
        Half_Chord_Distance : Float;
        Distance : Float;
        Intersection_Point : Vector;
        Surface_Normal : Vector;
        
    begin -- Intersect
      
        -- (1) Find if ray's origin is outside sphere
        Origin_To_Center := A_Sphere.Position - A_Ray.Origin;
        Length_Squared_Of_Origin_To_Center := Origin_To_Center * Origin_To_Center;
        Radius_Of_Sphere_Squared := A_Sphere.Radius * A_Sphere.Radius;
        Is_Outside := Length_Squared_Of_Origin_To_Center >= Radius_Of_Sphere_Squared;
      
        -- (2) Find closest approach of ray to sphere's center
        Closest_Approach := Origin_To_Center * A_Ray.Direction;
        Points_Away := Closest_Approach < 0.0;
      
        -- (3) If ray is outside and points away from sphere, ray must miss sphere
        if Is_Outside and Points_Away then
            return False;
        end if;
      
        -- (4) Else, find squared distance from closest approach to sphere surface
        Half_Chord_Distance_Squared := Radius_Of_Sphere_Squared -
          Length_Squared_Of_Origin_To_Center + Closest_Approach;

        -- (5) If value is negative, ray must miss sphere
        if Half_Chord_Distance_Squared < 0.0 then
            return False;
        end if;
      
        -- (6) Else, from above, find ray/surface distance
        Half_Chord_Distance := Sqrt(Half_Chord_Distance_Squared);
        Distance := (
                     if Is_Outside 
                     then Closest_Approach - Half_Chord_Distance 
                     else Closest_Approach + Half_Chord_Distance
                    );
      
        -- (7) Calculate intersection coordinate and surface normal
        Intersection_Point := Get_Point(A_Ray, Distance);
        Surface_Normal := Get_Surface_Normal(A_Sphere, Intersection_Point);
        A_Hit := (Intersection_Point => Intersection_Point, 
                  Surface_Normal => Surface_Normal); 
        return True;
      
    end Intersects;
    
end Ragnvaldr.Raytracer;
