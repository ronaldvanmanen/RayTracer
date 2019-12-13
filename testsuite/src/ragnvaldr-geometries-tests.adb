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
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;
with Ada.Exceptions;
with Ragnvaldr.Geometries;
  
package body Ragnvaldr.Geometries.Tests is

    type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

    procedure Test_Intersects_Ray_Outside_And_Points_Towards (T : in out Test_Fixture) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray
          (Origin => (1.0 * Meter, -2.0 * Meter, -1.0 * Meter),
           Direction => (1.0, 2.0, 4.0));
        
        A_Sphere : Sphere := 
          (Center => (3.0 * Meter, 0.0 * Meter, 5.0 * Meter),
           Radius => 3.0 * Meter);
        
        Actual_Hits : Hit_Array := Intersect(A_Ray => A_Ray, A_Sphere => A_Sphere);
        First_Hit : Hit;
        Second_Hit : Hit;
    
    begin -- Test_Intersects_Ray_Outside_And_Points_Towards
        
        Assert(Actual_Hits'Length = 2, "Expected the ray to hit the sphere twice");
        
        First_Hit := Actual_Hits(1);
        Assert(First_Hit.Distance = 3.74347758 * Meter, "Expected the distance of first hit to be 3.74347758");
        Assert(First_Hit.State = Entering, "Expected state of first hit is entering");
        
        Second_Hit := Actual_Hits(2);
        Assert(Second_Hit.Distance = 9.34959507 * Meter, "Expected the distance of the second hit to be 9.34959507");
        Assert(Second_Hit.State = Exiting, "Expected state of first hit is entering");

    end Test_Intersects_Ray_Outside_And_Points_Towards;

    procedure Test_Intersects_Ray_Outside_And_Points_Away (T : in out Test_Fixture) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray
          (Origin => (1.0 * Meter, -2.0 * Meter, -1.0 * Meter),
           Direction => (-1.0, -2.0, -4.0));
        
        A_Sphere : Sphere := 
          (Center => (3.0 * Meter, 0.0 * Meter, 5.0 * Meter),
           Radius => 3.0 * Meter);
        
        Actual_Hits : Hit_Array := Intersect(A_Ray => A_Ray, A_Sphere => A_Sphere);
    
    begin -- Test_Intersects_Ray_Outside_And_Points_Away

        Assert(Actual_Hits'Length = 0, "The ray must miss the sphere");
        
    end Test_Intersects_Ray_Outside_And_Points_Away;
    
    procedure Test_Intersects_Ray_Inside(T : in out Test_Fixture) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray
          (Origin => (3.0 * Meter, 0.0 * Meter, 5.0 * Meter),
           Direction => (1.0, 2.0, 4.0));
        
        A_Sphere : Sphere := 
          (Center => (3.0 * Meter, 0.0 * Meter, 5.0 * Meter),
           Radius => 3.0 * Meter);
        
        Actual_Hits : Hit_Array := Intersect(A_Ray => A_Ray, A_Sphere => A_Sphere);
        
        Actual_Hit : Hit;

    begin -- Test_Intersects_Ray_Inside
    
        Assert(Actual_Hits'Length = 1, "Expected the ray to hit the sphere once");
        
        Actual_Hit := Actual_Hits(1);
        Assert(Actual_Hit.Distance = 3.0 * Meter, "Expected the distance of the hit to be 3.0");
        Assert(Actual_Hit.State = Exiting, "Expected the ray to be exiting");
          
    end Test_Intersects_Ray_Inside;
      
    procedure Test_Intersects_Ray_Outside_And_Points_Along(T : in out Test_Fixture) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray
          (Origin => (0.0 * Meter, 0.0 * Meter, 0.0 * Meter),
           Direction => (0.0, 0.0, 1.0));
        
        A_Sphere : Sphere := 
          (Center => (1.0 * Meter, 1.0 * Meter, 1.0 * Meter),
           Radius => 1.0 * Meter);
        
        Actual_Hits : Hit_Array := Intersect(A_Ray => A_Ray, A_Sphere => A_Sphere);
    
    begin -- Test_Intersects_Ray_Outside_And_Points_Along

        Assert(Actual_Hits'Length = 0, "Expected the ray to miss the sphere");
        
    end Test_Intersects_Ray_Outside_And_Points_Along;
        
    package Caller is new AUnit.Test_Caller (Test_Fixture);

    function Suite return Access_Test_Suite is
        Ret : constant Access_Test_Suite := new Test_Suite;
    begin -- Suite
        Ret.Add_Test
          (Caller.Create
             ("Test_Intersects_Ray_Outside_And_Points_Towards",
              Test_Intersects_Ray_Outside_And_Points_Towards'Access));
        Ret.Add_Test
          (Caller.Create
             ("Test_Intersects_Ray_Outside_And_Points_Away",
              Test_Intersects_Ray_Outside_And_Points_Away'Access));
        Ret.Add_Test
          (Caller.Create(
           "Test_Intersects_Ray_Inside",
           Test_Intersects_Ray_Inside'Access));
        Ret.Add_Test
          (Caller.Create
             ("Test_Intersects_Ray_Outside_And_Points_Along",
              Test_Intersects_Ray_Outside_And_Points_Along'Access));
        return Ret;
    end Suite;

end Ragnvaldr.Geometries.Tests;
