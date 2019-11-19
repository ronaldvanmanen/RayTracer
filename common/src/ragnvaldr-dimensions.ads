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

generic

    type Real is digits <>;
    
package Ragnvaldr.Dimensions is

    -- The base type for all quantities as defined by International System of Quantities (ISQ).
    type SI_Type is new Real with
      Dimension_System => 
        (
         (Unit_Name => Meter,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),
         (Unit_Name => Kilogram, Unit_Symbol => "kg",  Dim_Symbol => 'M'),
         (Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),
         (Unit_Name => Ampere,   Unit_Symbol => 'A',   Dim_Symbol => 'I'),
         (Unit_Name => Kelvin,   Unit_Symbol => 'K',   Dim_Symbol => "Theta"),
         (Unit_Name => Mole,     Unit_Symbol => "mol", Dim_Symbol => 'N'),
         (Unit_Name => Candela,  Unit_Symbol => "cd",  Dim_Symbol => 'J')
        );

    -- The type for the base quantity length.
    subtype Length is SI_Type with Dimension => 
      (Symbol => 'm', Meter => 1, others => 0);
    
    -- The type for the base quantity mass.
    subtype Mass is SI_Type with Dimension =>
      (Symbol => "kg", Kilogram => 1, others => 0);
    
    -- The type for the base quantity time.
    subtype Time is SI_Type with Dimension => 
      (Symbol => 's', Second => 1, others => 0);

    -- The type for the base quantity electric current.
    subtype Electric_Current is SI_Type with Dimension => 
      (Symbol => 'A', Ampere => 1, others => 0);
    
    -- The type for the base quantity thermodynamic temperature.
    subtype Thermodynamic_Temperature is SI_Type with Dimension =>
      (Symbol => 'K', Kelvin => 1, others => 0);
    
    -- The type for the base quantity amount of substance.
    subtype Amount_Of_Substance is SI_Type with Dimension =>
      (Symbol => "mol", Mole => 1, others => 0);
    
    -- The type for the base quantity luminous intensity.
    subtype Luminous_Intensity is SI_Type with Dimension =>
      (Symbol => "cd", Candela => 1, others => 0);
    
    -- The type for the derived quantity frequency.
    subtype Frequency is SI_Type with Dimension =>
      (Symbol => "Hz", Second => -1, others => 0);
        
    -- The type for the derived quantity frequency.
    subtype Speed is SI_Type with Dimension =>
      (Symbol => "m/s", Meter => 1, Second => -1, others => 0);
    
    -- The type for the derived quantity acceleration.
    subtype Acceleration is SI_Type with Dimension =>
      (Symbol => "m/s^2", Meter => 1, Second => -2, others => 0);
    
    -- The type for the derived quantity area.
    subtype Area is SI_Type with Dimension => 
      (Symbol => 'm', Meter => 2, others => 0);
    
    -- The type for the derived quantity volume.
    subtype Volume is SI_Type with Dimension => 
      (Symbol => 'm', Meter => 3, others => 0);
        
    
    pragma Warnings (Off, "*assumed to be*");
    
    Yoctometer : constant Length := 1.0E-24;
    Zeptometer : constant Length := 1.0E-21;
    Attometer  : constant Length := 1.0E-18;
    Femtometer : constant Length := 1.0E-15;
    Picometer  : constant Length := 1.0E-12;
    Nanometer  : constant Length := 1.0E-09;
    Micrometer : constant Length := 1.0E-06;
    Millimeter : constant Length := 1.0E-03;
    Centimeter : constant Length := 1.0E-02;
    Decimeter  : constant Length := 1.0E-01;
    Meter      : constant Length := 1.0;
    Decameter  : constant Length := 1.0E+01;
    Hectometer : constant Length := 1.0E+02;
    Kilometer  : constant Length := 1.0E+03;
    Megameter  : constant Length := 1.0E+06;
    Gigameter  : constant Length := 1.0E+09;
    Terameter  : constant Length := 1.0E+12;
    Petameter  : constant Length := 1.0E+15;
    Exameter   : constant Length := 1.0E+18;
    Zettameter : constant Length := 1.0E+21;
    Yottameter : constant Length := 1.0E+24;

    Yoctogram : constant Mass := 1.0E-24;
    Zeptogram : constant Mass := 1.0E-21;
    Attogram  : constant Mass := 1.0E-18;
    Femtogram : constant Mass := 1.0E-15;
    Picogram  : constant Mass := 1.0E-12;
    Nanogram  : constant Mass := 1.0E-09;
    Microgram : constant Mass := 1.0E-06;
    Milligram : constant Mass := 1.0E-03;
    Centigram : constant Mass := 1.0E-02;
    Decigram  : constant Mass := 1.0E-01;
    Gram      : constant Mass := 1.0;
    Decagram  : constant Mass := 1.0E+01;
    Hectogram : constant Mass := 1.0E+02;
    Kilogram  : constant Mass := 1.0E+03;
    Megagram  : constant Mass := 1.0E+06;
    Gigagram  : constant Mass := 1.0E+09;
    Teragram  : constant Mass := 1.0E+12;
    Petagram  : constant Mass := 1.0E+15;
    Exagram   : constant Mass := 1.0E+18;
    Zettagram : constant Mass := 1.0E+21;
    Yottagram : constant Mass := 1.0E+24;

    Yoctosecond : constant Time := 1.0E-24;
    Zeptosecond : constant Time := 1.0E-21;
    Attosecond  : constant Time := 1.0E-18;
    Femtosecond : constant Time := 1.0E-15;
    Picosecond  : constant Time := 1.0E-12;
    Nanosecond  : constant Time := 1.0E-09;
    Microsecond : constant Time := 1.0E-06;
    Millisecond : constant Time := 1.0E-03;
    Centisecond : constant Time := 1.0E-02;
    Decisecond  : constant Time := 1.0E-01;
    Second      : constant Time := 1.0;
    Decasecond  : constant Time := 1.0E+01;
    Hectosecond : constant Time := 1.0E+02;
    Kilosecond  : constant Time := 1.0E+03;
    Megasecond  : constant Time := 1.0E+06;
    Gigasecond  : constant Time := 1.0E+09;
    Terasecond  : constant Time := 1.0E+12;
    Petasecond  : constant Time := 1.0E+15;
    Exasecond   : constant Time := 1.0E+18;
    Zettasecond : constant Time := 1.0E+21;
    Yottasecond : constant Time := 1.0E+24;

    pragma Warnings (On, "*assumed to be*");
    
end Ragnvaldr.Dimensions;
