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

with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ragnvaldr.Numerics.Generic_Array_Operations; use Ragnvaldr.Numerics.Generic_Array_Operations;

package body Ragnvaldr.Geometries is
    
    package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Scalar);
    use Elementary_Functions;
    
    package Instantiations is

        function "+" is new
          Vector_Vector_Elementwise_Operation
            (Left_Scalar   => Length,
             Right_Scalar  => Length,
             Result_Scalar => Length,
             Left_Vector   => Point,
             Right_Vector  => Displacement,
             Result_Vector => Point,
             Operation     => "+");

        function "-" is new
          Vector_Vector_Elementwise_Operation
            (Left_Scalar   => Length,
             Right_Scalar  => Length,
             Result_Scalar => Length,
             Left_Vector   => Point,
             Right_Vector  => Point,
             Result_Vector => Displacement,
             Operation     => "-");

    end Instantiations;

    function "+" (Left : Point; Right : Displacement) return Point renames
      Instantiations."+";

    function "-" (Left, Right : Point) return Displacement renames
      Instantiations."-";
    
    function Make_Ray(Origin : Point; Direction : Vector) return Ray is
    begin
        return Ray' (Origin => Origin, Direction => Direction / abs Direction);
    end;    
    
    function Get_Point (A_Ray : Ray;
                        A_Distance : Length) return Point is
    begin
        return A_Ray.Origin + A_Ray.Direction * A_Distance;
    end Get_Point;
    
    function Make_Sphere(Radius : Length) return Sphere is
    begin
        return Sphere'(Center => (0.0 * Meter,
                                  0.0 * Meter,
                                  0.0 * Meter), Radius => Radius);
    end Make_Sphere;      
        
    function Radius(A_Sphere : Sphere) return Length is
    begin
        return A_Sphere.Radius;
    end Radius;
        
    function Surface_Area(A_Sphere : Sphere) return Area is
    begin
        return 4.0 * Pi * A_Sphere.Radius * A_Sphere.Radius; 
    end Surface_Area;

    function Surface_Normal(A_Sphere : Sphere; 
                            A_Point : Point) return Vector is
    begin
        return (A_Point - A_Sphere.Center) / A_Sphere.Radius;
    end Surface_Normal;
    
    function Enclosed_Volume(S : Sphere) return Volume is
    begin
        return 4.0 / 3.0 * Pi * S.Radius * S.Radius * S.Radius;
    end Enclosed_Volume;

    function Intersect (A_Ray : in Ray;
                        A_Sphere : in Sphere) return Hit_Array is
      
        Origin_To_Center : Displacement (1..3);
        Length_Squared_Of_Origin_To_Center : Area;
        Radius_Of_Sphere_Squared : Area;
        Is_Outside : Boolean;
        Closest_Approach : Length;
        Points_Away : Boolean;
        Closest_Approach_Squared : Area;
        Half_Chord_Distance_Squared : Area;
        Half_Chord_Distance : Length;
        
    begin -- Intersect
      
        -- (1) Find if ray's origin is outside sphere
        Origin_To_Center := A_Sphere.Center - A_Ray.Origin;
        Length_Squared_Of_Origin_To_Center := Origin_To_Center * Origin_To_Center;
        Radius_Of_Sphere_Squared := A_Sphere.Radius * A_Sphere.Radius;
        Is_Outside := Length_Squared_Of_Origin_To_Center >= Radius_Of_Sphere_Squared;
      
        -- (2) Find closest approach of ray to sphere's center
        Closest_Approach := Origin_To_Center * A_Ray.Direction;
        Points_Away := Closest_Approach < 0.0;
      
        -- (3) If ray is outside and points away from sphere, ray must miss sphere
        if Is_Outside and Points_Away then
            return Empty_Hit_Array;
        end if;
      
        -- (4) Else, find squared distance from closest approach to sphere surface
        Closest_Approach_Squared := Closest_Approach * Closest_Approach;
        Half_Chord_Distance_Squared := Radius_Of_Sphere_Squared -
          Length_Squared_Of_Origin_To_Center + Closest_Approach_Squared;

        -- (5) If value is negative, ray must miss sphere
        if Half_Chord_Distance_Squared < 0.0 then
            return Empty_Hit_Array;
        end if;
      
        -- (6) Else, from above, find ray/surface distance
        Half_Chord_Distance := Sqrt(Half_Chord_Distance_Squared);

        if Is_Outside then
            declare
                Ret : Hit_Array(1..2);
            begin
                Ret (1) := Hit'
                  (Distance => Closest_Approach - Half_Chord_Distance,
                   State => Entering);
                Ret (2) := Hit'
                  (Distance => Closest_Approach + Half_Chord_Distance,
                   State => Exiting);
                return Ret;
            end;
        else
            declare
                Ret : Hit_Array(1..1);
            begin
                Ret (1) := Hit'
                  (Distance => Closest_Approach + Half_Chord_Distance,
                   State => Exiting);
                return Ret;
            end;
        end if;
    end Intersect;

end Ragnvaldr.Geometries;
