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

    procedure Test_Intersects (T : in out Test) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray(Origin => (1.0, -2.0, -1.0),
                                Direction => (1.0, 2.0, 4.0));
        
        A_Sphere : Sphere := (Position => (3.0, 0.0, 5.0),
                              Radius => 3.0);
        
        Actual_Intersections : Intersections := Intersects(A_Ray => A_Ray, A_Sphere => A_Sphere);
    begin -- Test_Intersects

        Assert(Actual_Intersections'Length = 2, "The ray must hit the sphere");
        Assert(Actual_Intersections(1) = 3.74347758, "The first intersection is at " & Float'Image(Actual_Intersections(1)));
        Assert(Actual_Intersections(2) = 9.34959507, "The second intersection is at " & Float'Image(Actual_Intersections(2)));

    end Test_Intersects;

    procedure Test_Not_Intersects (T : in out Test) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := Make_Ray(Origin => (1.0, -2.0, -1.0),
                                Direction => (-1.0, -2.0, -4.0));
        
        A_Sphere : Sphere := (Position => (3.0, 0.0, 5.0),
                              Radius => 3.0);
        
        Actual_Intersections : Intersections := Intersects(A_Ray => A_Ray, A_Sphere => A_Sphere);
    begin -- Test_Intersects

        Assert(Actual_Intersections'Length = 0, "The ray must not hit the sphere");
        
    end Test_Not_Intersects;

end Ragnvaldr.Raytracer.Test;
