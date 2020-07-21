--  Ragnvaldr Systems -- Raytracer
--
--  Copyright (C) 2020  Ronald van Manen
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
with Ragnvaldr.Numerics.Rationals;

package body Ragnvaldr.Numerics.Generic_Rationals.Tests is

    type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

    procedure Addition_Satisfies_Additive_Associativity (T : in out Test_Fixture) is
        pragma Unreferenced(T);
        A : Rational := (Numerator => 1, Denominator => 3);
        B : Rational := (Numerator => 11, Denominator => 13);
        C : Rational := (Numerator => 23, Denominator => 29);
    begin
        Assert(A + (B + C) = (A + B) + C, "Addition must satify additive associativity");
    end Addition_Satisfies_Additive_Associativity;

    procedure Addition_Satisfies_Additive_Commutativity (T : in out Test_Fixture) is
        pragma Unreferenced(T);
        A : Rational := (Numerator => 1, Denominator => 3);
        B : Rational := (Numerator => 11, Denominator => 13);
    begin
        Assert(A + B = B + A, "Addition must satify additive commutativity");
    end Addition_Satisfies_Additive_Commutativity;
        
    procedure Addition_Satisfies_Additive_Identity (T : in out Test_Fixture) is
        pragma Unreferenced(T);
        A : Rational := (Numerator => 1, Denominator => 3);
        Zero : Rational := (Numerator => 0, Denominator => 1);
    begin
        Assert(A = A + Zero, "Addition must satify additive identity");
        Assert(A = Zero + A, "Addition must satify additive identity");
    end Addition_Satisfies_Additive_Identity;
    
    procedure Multiplication_Satisfies_Multiplicative_Associativity (T : in out Test_Fixture) is
        pragma Unreferenced(T);
        A : Rational := (Numerator => 1, Denominator => 3);
        B : Rational := (Numerator => 11, Denominator => 13);
        C : Rational := (Numerator => 23, Denominator => 29);
    begin
        Assert(A * (B * C) = (A * B) * C, "Multiplication must satify multiplicative associativity");
    end Multiplication_Satisfies_Multiplicative_Associativity;

    procedure Multiplication_Satisfies_Multiplicative_Identity (T : in out Test_Fixture) is
        pragma Unreferenced(T);
        A : Rational := (Numerator => 1, Denominator => 3);
        One : Rational := (Numerator => 1, Denominator => 1);
    begin
        Assert(A = A * One, "Multiplication must satisfy multiplicative identity");
        Assert(A = One * A, "Multiplication must satisfy multiplicative identity");
    end Multiplication_Satisfies_Multiplicative_Identity;
    
    package Caller is new AUnit.Test_Caller (Test_Fixture);

    function Suite return Access_Test_Suite is
        Ret : constant Access_Test_Suite := new Test_Suite;
    begin -- Suite
        Ret.Add_Test
          (Caller.Create
             ("Addition_Satisfies_Additive_Associativity", 
              Addition_Satisfies_Additive_Associativity'Access));
        Ret.Add_Test
          (Caller.Create
             ("Addition_Satisfies_Additive_Commutativity", 
              Addition_Satisfies_Additive_Commutativity'Access));
        Ret.Add_Test
          (Caller.Create
             ("Addition_Satisfies_Additive_Identity", 
              Addition_Satisfies_Additive_Identity'Access));
        Ret.Add_Test
          (Caller.Create
             ("Multiplication_Satisfies_Multiplicative_Associativity",
              Multiplication_Satisfies_Multiplicative_Associativity'Access));
        Ret.Add_Test
          (Caller.Create
             ("Multiplication_Satisfies_Multiplicative_Identity",
              Multiplication_Satisfies_Multiplicative_Identity'Access));
        return Ret;
    end Suite;

end Ragnvaldr.Numerics.Generic_Rationals.Tests;
