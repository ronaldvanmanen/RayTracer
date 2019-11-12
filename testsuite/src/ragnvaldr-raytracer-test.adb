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

end Ragnvaldr.Raytracer.Test;
