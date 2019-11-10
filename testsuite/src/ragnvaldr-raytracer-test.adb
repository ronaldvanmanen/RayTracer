with AUnit.Assertions; use AUnit.Assertions;
with Ada.Exceptions;
with Ragnvaldr.Raytracer;
  
package body Ragnvaldr.Raytracer.Test is

    procedure Test_Intersects (T : in out Test) is
        
        pragma Unreferenced(T);

        A_Ray : Ray := (Origin => (1.0, -2.0, 1.0), Direction => (1.0, 2.0, 4.0));
        A_Sphere : Sphere := (Position => (3.0, 0.0, 5.0), Radius => 3.0);
        Is_Hit : Boolean := False;
        A_Hit : Hit;
    
    begin -- Test_Intersects
    
        Is_Hit := Intersects(A_Ray => A_Ray, A_Sphere => A_Sphere, A_Hit => A_Hit);
        
        Assert(Is_Hit, "The ray must hit the sphere");
    
    end Test_Intersects;

end Ragnvaldr.Raytracer.Test;
