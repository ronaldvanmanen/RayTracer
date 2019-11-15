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

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Exceptions;
with Ragnvaldr.Raytracer;
  
package body Ragnvaldr.Raytracer.Test is

    procedure Test_Intersects_Ray_Outside_And_Points_Towards (T : in out Test) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray(Origin => (1.0, -2.0, -1.0),
                                Direction => (1.0, 2.0, 4.0));
        
        A_Sphere : Sphere := (Position => (3.0, 0.0, 5.0),
                              Radius => 3.0);
        
        Actual_Intersections : Intersections := Intersects(A_Ray => A_Ray, A_Sphere => A_Sphere);
    
    begin -- Test_Intersects_Ray_Outside_And_Points_Towards

        Assert(Actual_Intersections'Length = 2, "The ray must hit the sphere twice");
        Assert(Actual_Intersections(1) = 3.74347758, "The first hit is at " & Float'Image(Actual_Intersections(1)));
        Assert(Actual_Intersections(2) = 9.34959507, "The second hit is at " & Float'Image(Actual_Intersections(2)));

    end Test_Intersects_Ray_Outside_And_Points_Towards;

    procedure Test_Intersects_Ray_Outside_And_Points_Away (T : in out Test) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray(Origin => (1.0, -2.0, -1.0),
                                Direction => (-1.0, -2.0, -4.0));
        
        A_Sphere : Sphere := (Position => (3.0, 0.0, 5.0),
                              Radius => 3.0);
        
        Actual_Intersections : Intersections := Intersects(A_Ray => A_Ray, A_Sphere => A_Sphere);
    
    begin -- Test_Intersects_Ray_Outside_And_Points_Away

        Assert(Actual_Intersections'Length = 0, "The ray must miss the sphere");
        
    end Test_Intersects_Ray_Outside_And_Points_Away;
    
    procedure Test_Intersects_Ray_Inside(T : in out Test) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray(Origin => (3.0, 0.0, 5.0),
                                Direction => (1.0, 2.0, 4.0));
        
        A_Sphere : Sphere := (Position => (3.0, 0.0, 5.0),
                              Radius => 3.0);
        
        Actual_Intersections : Intersections := Intersects(A_Ray => A_Ray, A_Sphere => A_Sphere);

    begin -- Test_Intersects_Ray_Inside
    
        Assert(Actual_Intersections'Length = 1, "The ray must hit the sphere once");
        Assert(Actual_Intersections(1) = 3.0, "The hit is at " & Float'Image(Actual_Intersections(1)));
    
    end Test_Intersects_Ray_Inside;
      
    procedure Test_Intersects_Ray_Outside_And_Points_Along(T : in out Test) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray(Origin => (0.0, 0.0, 0.0),
                                Direction => (0.0, 0.0, 1.0));
        
        A_Sphere : Sphere := (Position => (1.0, 1.0, 1.0),
                              Radius => 1.0);
        
        Actual_Intersections : Intersections := Intersects(A_Ray => A_Ray, A_Sphere => A_Sphere);
    
    begin -- Test_Intersects_Ray_Outside_And_Points_Along

        Assert(Actual_Intersections'Length = 0, "The ray must miss the sphere");
        
    end Test_Intersects_Ray_Outside_And_Points_Along;

end Ragnvaldr.Raytracer.Test;
