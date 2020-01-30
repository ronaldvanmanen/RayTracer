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

with Ragnvaldr.Dimensions.Tests;
with Ragnvaldr.Geometries.Tests;
with Ragnvaldr.Numerics.Generic_Real_Quaternions.Tests;
with Ragnvaldr.Numerics.Generic_Real_Vectors.Tests;
with AUnit.Tests;

package body Ragnvaldr.Tests is

    package Real_Quaternions is 
      new Ragnvaldr.Numerics.Generic_Real_Quaternions(Float);
    package Real_Quaternions_Tests is new Real_Quaternions.Tests;
    
    package Real_Vectors is 
      new Ragnvaldr.Numerics.Generic_Real_Vectors(Float);
    package Real_Vectors_Tests is new Real_Vectors.Tests;
    
    function Suite return Access_Test_Suite is
        Result : Access_Test_Suite := AUnit.Test_Suites.New_Suite;
    begin
        Result.Add_Test (Ragnvaldr.Dimensions.Tests.Suite);
        Result.Add_Test (Ragnvaldr.Geometries.Tests.Suite);
        Result.Add_Test (Real_Quaternions_Tests.Suite);
        Result.Add_Test (Real_Vectors_Tests.Suite);
        return Result;
    end Suite;
end Ragnvaldr.Tests;
