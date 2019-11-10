with AUnit.Test_Fixtures;

package Ragnvaldr.Raytracer.Test
  with
    SPARK_Mode => Off
  is

    type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

    procedure Test_Intersects (T : in out Test);

end Ragnvaldr.Raytracer.Test;
