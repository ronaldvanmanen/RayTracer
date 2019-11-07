with AUnit.Reporter.Text;
with AUnit.Run;
with Ragnvaldr.Raytracer.Test.Suite; use Ragnvaldr.Raytracer.Test.Suite;

procedure Ragnvaldr.Raytracer.Test_Runner is
    procedure Runner is new AUnit.Run.Test_Runner (Suite);
    Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
    Runner (Reporter);
end Ragnvaldr.Raytracer.Test_Runner;
