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

package Ragnvaldr.Dimensions is

    pragma Pure (Dimensions);
    
    -- The base type for all quantities as defined by International System of Quantities (ISQ).
    type Scalar is new Float with
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
    
    -- The type for the base quantity amount of substance.
    subtype Amount_Of_Substance is Scalar with Dimension =>
      (Symbol => "mol", Mole => 1, others => 0);
    
    -- The type for the base quantity electric current.
    subtype Electric_Current is Scalar with Dimension => 
      (Symbol => 'A', Ampere => 1, others => 0);
    
    -- The type for the base quantity length.
    subtype Length is Scalar with Dimension => 
      (Symbol => 'm', Meter => 1, others => 0);

    -- The type for the base quantity luminous intensity.
    subtype Luminous_Intensity is Scalar with Dimension =>
      (Symbol => "cd", Candela => 1, others => 0);
    
    -- The type for the base quantity mass.
    subtype Mass is Scalar with Dimension =>
      (Symbol => "kg", Kilogram => 1, others => 0);
    
    -- The type for the base quantity thermodynamic temperature.
    subtype Thermodynamic_Temperature is Scalar with Dimension =>
      (Symbol => 'K', Kelvin => 1, others => 0);
    
    -- The type for the base quantity time.
    subtype Time is Scalar with Dimension => 
      (Symbol => 's', Second => 1, others => 0);
    
    -- The type for the derived quantity absorbed dose.
    subtype Absorbed_Dose is Scalar with Dimension =>
      (Symbol => "Gy", Meter => 2, Second => -2, others => 0);
    
    -- The type for the derived quantity angle.
    subtype Angle is Scalar with Dimension =>
      (Symbol => "rad", others => 0);
             
    -- The type for the derived quantity acceleration.
    subtype Acceleration is Scalar with Dimension =>
      (Symbol => "m/s^2", Meter => 1, Second => -2, others => 0);
    
    -- The type for the derived quantity area.
    subtype Area is Scalar with Dimension => 
      (Symbol => 'm', Meter => 2, others => 0);

    -- The type for the derived quantity catalytic activity.
    subtype Catalytic_Activity is Scalar with Dimension =>
      (Symbol => "kat", Second => -1, Mole => 1, others => 0);

    -- The type for the derived quantity electric charge.
    subtype Electric_Charge is Scalar with Dimension =>
      (Symbol => 'C', Second => 1, Ampere => 1, others => 0);

    -- The type for the derived quantity electrical capacitance.
    subtype Electrical_Capacitance is Scalar with Dimension =>
      (Symbol => 'F', Kilogram => -1, Meter => -2, Second => 4, Ampere => 2, others => 0);

    -- The type for the derived quantity electrical conductance.
    subtype Electrical_Conductance is Scalar with Dimension =>
      (Symbol => "S", Kilogram => -1, Meter => -2, Second => 3, Ampere => 2, others => 0);

    -- The type for the derived quantity electrical inductance.
    subtype Electrical_Inductance is Scalar with Dimension =>
      (Symbol => "S", Kilogram => 1, Meter => 2, Second => -2, Ampere => -2, others => 0);

    -- The type for the derived quantity electrical resistance.
    subtype Electrical_Resistance is Scalar with Dimension =>
      (Symbol => "Ω", Kilogram => 1, Meter => 2, Second => -3, Ampere => -2, others => 0);
    
    -- The type for the derived quantity electrical potential difference.
    subtype Electrical_Potential_Difference is Scalar with Dimension =>
      (Symbol => 'V', Kilogram => 1, Meter => 2, Second => -3, Ampere => -1, others => 0);
    
    -- The type for the derived quantity electromotive force.
    subtype Electromotive_Force is Scalar with Dimension =>
      (Symbol => 'V', Kilogram => 1, Meter => 2, Second => -3, Ampere => -1, others => 0);

    -- The type for the derived quantity equivalent dose.
    subtype Equivalent_Dose is Scalar with Dimension =>
      (Symbol => "Sv", Meter => 2, Second => -2, others => 0);

    -- The type for the derived quantity energy.
    subtype Energy is Scalar with Dimension =>
      (Symbol => 'J', Kilogram => 1, Meter => 2, Second => -2, others => 0);

    -- The type for the derived quantity force.
    subtype Force is Scalar with Dimension =>
      (Symbol => 'N', Kilogram => 1, Meter => 1, Second => -2, others => 0);
    
    -- The type for the derived quantity frequency.
    subtype Frequency is Scalar with Dimension =>
      (Symbol => "Hz", Second => -1, others => 0);

    -- The type for the derived quantity heat.
    subtype Heat is Scalar with Dimension =>
      (Symbol => 'J', Kilogram => 1, Meter => 2, Second => -2, others => 0);

    -- The type for the derived quantity illuminance.
    subtype Illuminance is Scalar with Dimension =>
      (Symbol => "lm", Candela => 1, Meter => -2, others => 0);

    -- The type for the derived quantity impedance.
    subtype Impedance is Scalar with Dimension =>
      (Symbol => "Ω", Kilogram => 1, Meter => 2, Second => -3, Ampere => -2, others => 0);

    -- The type for the derived quantity luminous flux.
    subtype Luminous_Flux is Scalar with Dimension =>
      (Symbol => "lm", Candela => 1, others => 0);
    
    -- The type for the derived quantity magnetic flux.
    subtype Magnetic_Flux is Scalar with Dimension =>
      (Symbol => "Wb", Kilogram => 1, Meter => 2, Second => -2, Ampere => -1, others => 0);
    
    -- The type for the derived quantity magnetic flux density.
    subtype Magnetic_Flux_Density is Scalar with Dimension =>
      (Symbol => "Wb", Kilogram => 1, Second => -2, Ampere => -1, others => 0);

    -- The type for the derived quantity magnetic induction.
    subtype Magnetic_Induction is Scalar with Dimension =>
      (Symbol => "Wb", Kilogram => 1, Second => -2, Ampere => -1, others => 0);

    -- The type for the derived quantity power.
    subtype Power is Scalar with Dimension =>
      (Symbol => 'W', Kilogram => 1, Meter => 2, Second => -3, others => 0);

    -- The type for the derived quantity pressure.
    subtype Pressure is Scalar with Dimension =>
      (Symbol => "Pa", Kilogram => 1, Meter => -1, Second => -2, others => 0);
    
    -- The type for the derived quantity quantity of electricity.
    subtype Quantity_Of_Electricity is Scalar with Dimension =>
      (Symbol => 'C', Second => 1, Ampere => 1, others => 0);
    
    -- The type for the derived quantity radiant flux.
    subtype Radiant_Flux is Scalar with Dimension =>
      (Symbol => 'W', Kilogram => 1, Meter => 2, Second => -3, others => 0);
    
    -- The type for the derived quantity radioactivity.
    subtype Radioactivity is Scalar with Dimension =>
      (Symbol => "Bq", Second => -1, others => 0);

    -- The type for the derived quantity reactance.
    subtype Reactance is Scalar with Dimension =>
      (Symbol => "Ω", Kilogram => 1, Meter => 2, Second => -3, Ampere => -2, others => 0);

    -- The type for the derived quantity solid angle.
    subtype Solid_Angle is Scalar with Dimension =>
      (Symbol => "sr", others => 0);
    
    -- The type for the derived quantity speed.
    subtype Speed is Scalar with Dimension =>
      (Symbol => "m/s", Meter => 1, Second => -1, others => 0);

    -- The type for the derived quantity stress.
    subtype Stress is Scalar with Dimension =>
      (Symbol => "Pa", Kilogram => 1, Meter => -1, Second => -2, others => 0);
    
    -- The type for the derived quantity velocity.
    subtype Velocity is Scalar with Dimension =>
      (Symbol => "m/s", Meter => 1, Second => -1, others => 0);
    
    -- The type for the derived quantity voltage.
    subtype Voltage is Scalar with Dimension =>
      (Symbol => 'V', Kilogram => 1, Meter => 2, Second => -3, Ampere => -1, others => 0);
    
    -- The type for the derived quantity volume.
    subtype Volume is Scalar with Dimension => 
      (Symbol => 'm', Meter => 3, others => 0);

    -- The type for the derived quantity weight.
    subtype Weight is Scalar with Dimension =>
      (Symbol => 'N', Kilogram => 1, Meter => 1, Second => -2, others => 0);
    
    -- The type for the derived quantity work.
    subtype Work is Scalar with Dimension =>
      (Symbol => 'J', Kilogram => 1, Meter => 2, Second => -2, others => 0);
        
    pragma Warnings (Off, "*assumed to be*");

    Yoctomole : constant Amount_Of_Substance := 1.0E-24;
    Zeptomole : constant Amount_Of_Substance := 1.0E-21;
    Attomole  : constant Amount_Of_Substance := 1.0E-18;
    Femtomole : constant Amount_Of_Substance := 1.0E-15;
    Picomole  : constant Amount_Of_Substance := 1.0E-12;
    Nanomole  : constant Amount_Of_Substance := 1.0E-09;
    Micromole : constant Amount_Of_Substance := 1.0E-06;
    Millimole : constant Amount_Of_Substance := 1.0E-03;
    Centimole : constant Amount_Of_Substance := 1.0E-02;
    Decimole  : constant Amount_Of_Substance := 1.0E-01;
    Mole      : constant Amount_Of_Substance := 1.0;
    Decamole  : constant Amount_Of_Substance := 1.0E+01;
    Hectomole : constant Amount_Of_Substance := 1.0E+02;
    Kilomole  : constant Amount_Of_Substance := 1.0E+03;
    Megamole  : constant Amount_Of_Substance := 1.0E+06;
    Gigamole  : constant Amount_Of_Substance := 1.0E+09;
    Teramole  : constant Amount_Of_Substance := 1.0E+12;
    Petamole  : constant Amount_Of_Substance := 1.0E+15;
    Examole   : constant Amount_Of_Substance := 1.0E+18;
    Zettamole : constant Amount_Of_Substance := 1.0E+21;
    Yottamole : constant Amount_Of_Substance := 1.0E+24;
    
    Yoctoampere : constant Electric_Current := 1.0E-24;
    Zeptoampere : constant Electric_Current := 1.0E-21;
    Attoampere  : constant Electric_Current := 1.0E-18;
    Femtoampere : constant Electric_Current := 1.0E-15;
    Picoampere  : constant Electric_Current := 1.0E-12;
    Nanoampere  : constant Electric_Current := 1.0E-09;
    Microampere : constant Electric_Current := 1.0E-06;
    Milliampere : constant Electric_Current := 1.0E-03;
    Centiampere : constant Electric_Current := 1.0E-02;
    Deciampere  : constant Electric_Current := 1.0E-01;
    Ampere      : constant Electric_Current := 1.0;
    Decaampere  : constant Electric_Current := 1.0E+01;
    Hectoampere : constant Electric_Current := 1.0E+02;
    Kiloampere  : constant Electric_Current := 1.0E+03;
    Megaampere  : constant Electric_Current := 1.0E+06;
    Gigaampere  : constant Electric_Current := 1.0E+09;
    Teraampere  : constant Electric_Current := 1.0E+12;
    Petaampere  : constant Electric_Current := 1.0E+15;
    Exaampere   : constant Electric_Current := 1.0E+18;
    Zettaampere : constant Electric_Current := 1.0E+21;
    Yottaampere : constant Electric_Current := 1.0E+24;
    
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

    Yoctocandela : constant Luminous_Intensity := 1.0E-24;
    Zeptocandela : constant Luminous_Intensity := 1.0E-21;
    Attocandela  : constant Luminous_Intensity := 1.0E-18;
    Femtocandela : constant Luminous_Intensity := 1.0E-15;
    Picocandela  : constant Luminous_Intensity := 1.0E-12;
    Nanocandela  : constant Luminous_Intensity := 1.0E-09;
    Microcandela : constant Luminous_Intensity := 1.0E-06;
    Millicandela : constant Luminous_Intensity := 1.0E-03;
    Centicandela : constant Luminous_Intensity := 1.0E-02;
    Decicandela  : constant Luminous_Intensity := 1.0E-01;
    Candela      : constant Luminous_Intensity := 1.0;
    Decacandela  : constant Luminous_Intensity := 1.0E+01;
    Hectocandela : constant Luminous_Intensity := 1.0E+02;
    Kilocandela  : constant Luminous_Intensity := 1.0E+03;
    Megacandela  : constant Luminous_Intensity := 1.0E+06;
    Gigacandela  : constant Luminous_Intensity := 1.0E+09;
    Teracandela  : constant Luminous_Intensity := 1.0E+12;
    Petacandela  : constant Luminous_Intensity := 1.0E+15;
    Exacandela   : constant Luminous_Intensity := 1.0E+18;
    Zettacandela : constant Luminous_Intensity := 1.0E+21;
    Yottacandela : constant Luminous_Intensity := 1.0E+24;

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

    Yoctokelvin : constant Thermodynamic_Temperature := 1.0E-24;
    Zeptokelvin : constant Thermodynamic_Temperature := 1.0E-21;
    Attokelvin  : constant Thermodynamic_Temperature := 1.0E-18;
    Femtokelvin : constant Thermodynamic_Temperature := 1.0E-15;
    Picokelvin  : constant Thermodynamic_Temperature := 1.0E-12;
    Nanokelvin  : constant Thermodynamic_Temperature := 1.0E-09;
    Microkelvin : constant Thermodynamic_Temperature := 1.0E-06;
    Millikelvin : constant Thermodynamic_Temperature := 1.0E-03;
    Centikelvin : constant Thermodynamic_Temperature := 1.0E-02;
    Decikelvin  : constant Thermodynamic_Temperature := 1.0E-01;
    Kelvin      : constant Thermodynamic_Temperature := 1.0;
    Decakelvin  : constant Thermodynamic_Temperature := 1.0E+01;
    Hectokelvin : constant Thermodynamic_Temperature := 1.0E+02;
    Kilokelvin  : constant Thermodynamic_Temperature := 1.0E+03;
    Megakelvin  : constant Thermodynamic_Temperature := 1.0E+06;
    Gigakelvin  : constant Thermodynamic_Temperature := 1.0E+09;
    Terakelvin  : constant Thermodynamic_Temperature := 1.0E+12;
    Petakelvin  : constant Thermodynamic_Temperature := 1.0E+15;
    Exakelvin   : constant Thermodynamic_Temperature := 1.0E+18;
    Zettakelvin : constant Thermodynamic_Temperature := 1.0E+21;
    Yottakelvin : constant Thermodynamic_Temperature := 1.0E+24;

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
    
    radian      : constant Angle := 1.0;
    steradian   : constant Solid_Angle := 1.0;
    hertz       : constant Frequency := 1.0;
    newton      : constant Force := 1.0;
    pascal      : constant Pressure := 1.0;
    joule       : constant Energy := 1.0;
    watt        : constant Power := 1.0;
    coulomb     : constant Electric_Charge := 1.0;
    volt        : constant Voltage := 1.0;
    farad       : constant Electrical_Capacitance := 1.0;
    ohm         : constant Electrical_Resistance := 1.0;
    siemens     : constant Electrical_Conductance := 1.0;
    weber       : constant Magnetic_Flux := 1.0;
    tesla       : constant Magnetic_Flux_Density := 1.0;
    henry       : constant Electrical_Inductance := 1.0;
    lumen       : constant Luminous_Flux := 1.0;
    lux         : constant Illuminance := 1.0;
    becquerel   : constant Radioactivity := 1.0;
    gray        : constant Absorbed_Dose := 1.0;
    sievert     : constant Equivalent_Dose := 1.0;
    katal       : constant Catalytic_Activity := 1.0;

    pragma Warnings (On, "*assumed to be*");
            
    type Vector is array(Integer range <>) of Scalar;
        
    type Displacement is array(Integer range <>) of Length;
    
    function "*" (Left : Vector; Right : Length) return Displacement;
    
    function "/" (Left : Displacement; Right : Length) return Vector;
    
    function "+" (Left, Right : Displacement) return Displacement;
    
    function "-" (Left, Right : Displacement) return Displacement;
    
end Ragnvaldr.Dimensions;
