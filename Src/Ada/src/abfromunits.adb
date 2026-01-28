with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals; use Ada.Numerics.Big_Numbers.Big_Reals;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Arg_Parser; use Arg_Parser;

procedure ABFromUnits is
   Program_Name : String :=
     (if Command_Name = "" then "abfromunits" else Command_Name);

   Exit_Program : exception;

   procedure Error (Message : String) is
   begin
      Put_Line (Standard_Error, Program_Name & ": error: " & Message);
      Flush (Standard_Error);
   end Error;

   procedure Fatal_Error (Exit_Code : Natural; Message : String) is
   begin
      Put (Standard_Error, Program_Name & ": fatal error: ");
      Put_Line (Standard_Error, Message);
      Flush (Standard_Error);
      OS_Exit (Exit_Code);
   end Fatal_Error;


   function "+" (Right : String) return Unbounded_String renames
     To_Unbounded_String;
   function "+" (Right : Unbounded_String) return String renames To_String;

   type Multiplier is ('K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y', 'R', 'Q');

   package Multiplier_IO is new Ada.Text_IO.Enumeration_IO (Enum => Multiplier);
   use Multiplier_IO;

   type Labeled_Multiplier is record
      SI_Label : Unbounded_String;
      SI_Abbreviation : Unbounded_String;
      SI_Text : Unbounded_String;
      SI : Big_Real;
      BI_Label : Unbounded_String;
      BI_Abbreviation : Unbounded_String;
      BI_Text : Unbounded_String;
      BI : Big_Real;
   end record;

   type Labeled_Multiplier_Array is array (Multiplier range <>)
     of Labeled_Multiplier;

   Use_SI_Default : constant Boolean := False;
   Use_SI : aliased Boolean := Use_SI_Default;

   Multipliers : Labeled_Multiplier_Array :=
     ('K' => (+"Kilo",   +"K", +"10.0**03", 10.0**03, +"Kibi", +"Ki", +"2.0**010", 2.0**010),
      'M' => (+"Mega",   +"M", +"10.0**06", 10.0**06, +"Mebi", +"Mi", +"2.0**020", 2.0**020),
      'G' => (+"Giga",   +"G", +"10.0**09", 10.0**09, +"Gibi", +"Gi", +"2.0**030", 2.0**030),
      'T' => (+"Tera",   +"T", +"10.0**12", 10.0**12, +"Tebi", +"Ti", +"2.0**040", 2.0**040),
      'P' => (+"Peta",   +"P", +"10.0**15", 10.0**15, +"Pebi", +"Pi", +"2.0**050", 2.0**050),
      'E' => (+"Exa",    +"E", +"10.0**18", 10.0**18, +"Exbi", +"Ei", +"2.0**060", 2.0**060),
      'Z' => (+"Zeta",   +"Z", +"10.0**21", 10.0**21, +"Zebi", +"Zi", +"2.0**070", 2.0**070),
      'Y' => (+"Yotta",  +"Y", +"10.0**24", 10.0**24, +"Yobi", +"Yi", +"2.0**080", 2.0**080),
      'R' => (+"Ronna",  +"R", +"10.0**27", 10.0**27, +"Robi", +"Ri", +"2.0**090", 2.0**090),
      'Q' => (+"Quetta", +"Q", +"10.0**30", 10.0**30, +"Qubi", +"Qi", +"2.0**100", 2.0**100));

   function By_Multiplier (M : Multiplier) return Big_Real is
     (if Use_SI then Multipliers (M).SI else Multipliers (M).BI);

   function Abbreviation (M: Multiplier) return Unbounded_String is
     (if Use_SI then Multipliers (M).SI_Abbreviation
      else Multipliers (M).BI_Abbreviation);

   procedure Maybe_Multiply (R : in out Big_Real; S: String) is
      T : Unbounded_String := +S;
   begin
      for I in Multiplier'Range loop
         if T = Multipliers(I).SI_Abbreviation then
            R := R * By_Multiplier (I);
            return;
         end if;
      end loop;
   end Maybe_Multiply;

   procedure Print_Binary_Prefixes is
   begin
      for C in Multipliers'Range loop
         declare
            E : Labeled_Multiplier renames Multipliers (C);
         begin
            Put_Line (Head (+E.BI_Label & ": ", 8) & Trim (To_String (E.BI, Aft => 1), Both));
         end;
      end loop;
   end Print_Binary_Prefixes;

   procedure Print_SI_Prefixes is
   begin
      for C in Multipliers'Range loop
         declare
            E : Labeled_Multiplier renames Multipliers (C);
         begin
            Put_Line (Head (+E.SI_Label & ": ", 8) & Trim (To_String (E.SI, Aft => 1), Both));
         end;
      end loop;
   end Print_SI_Prefixes;

   function Print_Prefixes return Boolean is
   begin
      Put_Line ("SI/Metric Prefixes: https://en.wikipedia.org/wiki/Metric_prefix");
      for C in Multipliers'Range loop
         declare
            E: Labeled_Multiplier renames Multipliers (C);
         begin
            Put_Line (Head (+E.SI_Label, 6) & " (" & (+E.SI_Abbreviation) & ")  " & (+E.Si_Text) & " " & Trim (To_String (E.SI, Aft => 1), Both));
         end;
      end loop;
      New_Line;

      Put_Line ("Binary Prefixes: https://en.wikipedia.org/wiki/Binary_prefix");
      for C in Multipliers'Range loop
         declare
            E: Labeled_Multiplier renames Multipliers (C);
         begin
            Put_Line (Head (+E.BI_Label, 6) & " (" & (+E.BI_Abbreviation) & ") " & (+E.BI_Text) & " " & Trim (To_String (E.BI, Aft => 1), Both));
         end;
      end loop;

      return True;
   end Print_Prefixes;

   function Relaxed_From_String (S: String) return Big_Real is
   begin
      declare
         R: Big_Real := From_String (S);
      begin
         return R;
      end;
   exception
      when CONSTRAINT_ERROR =>
         begin
            declare
               I : Big_Integer := From_String (S);
               R : Big_Real := To_Big_Real (I);
            begin
               return R;
            end;
         exception
            when CONSTRAINT_ERROR =>
               Error ("The value """ & S & """ is not a valid floating point number.");
               raise;
         end;
   end Relaxed_From_String;

   Units : aliased Unbounded_String := +"B"; -- Units default to bytes.

   Arguments_Seen : Natural := -0;

   function Process_Argument (Start_With : Positive; Arg: String) return Boolean is
      S : String := Trim (Arg, Both);
      F : constant Integer := S'First;
      L : constant Integer := S'Last;
      I : Integer := S'First;
      R : Big_Real;
   begin
      Arguments_Seen := Arguments_Seen + 1;
      -- Ugh.  This is so ugly.  And not really correct.
      while I <= L and then (Is_Digit (S(I))
                             or else S(I) = '_' or else S(I) = '.'
                             or else S(I) = '+' or else S(I) = '-'
                             or else S(I) = 'e' or else S(I) = 'E'
                             or else S(I) = '#')
      loop
         I := I + 1;
      end loop;

      R := Relaxed_From_String (S(F..I - 1));
      if I <= L then
         Maybe_Multiply (R, S(I..I));
      end if;
      Put_Line (To_String (R, Aft => 1));
      return True;
   end Process_Argument;

   procedure Process_Standard_Input is
      Continue : Boolean := True;
   begin
      loop
         exit when End_Of_File or not Continue;
         Continue := Process_Argument (1, Get_Line);
      end loop;
   end Process_Standard_Input;

   function Print_Usage return Boolean;

   Options : aliased Option_Array :=
     (Make_Set_Boolean_False_Option
        ("Use the binary prefixes for units.", 'b', "binary", Use_SI'Unrestricted_Access),
      Make_Option
        ("Display the help message.", 'h', "help", Print_Usage'Unrestricted_access),
      Make_Option
        ("Print the prefixes.", 'p', "prefixes", Print_Prefixes'Unrestricted_access),
      Make_Set_Boolean_True_Option
        ("Use the SI prefixes for units.", 's', "si", Use_Si'Unrestricted_Access),
      Make_Set_Unbounded_String_Option
        ("Set the units to use.", 'u', "units", Units'Unrestricted_access));

   AP : Argument_Parser :=
     Make_Argument_Parser ("afromunits [options] [argument ...]", Process_Argument'Unrestricted_Access,
                           Options'Unrestricted_Access);
   function Print_Usage return Boolean is
      Default : String := (if Use_SI_Default then "SI" else "binary");
   begin
      Usage (AP);
      New_Line;
      Put_Line ("This program defaults to using the " & Default'Image & " prefixes for units.");
      raise Exit_Program;
      return False;             -- If they ask for help, it's time to stop parsing arguments.
   end Print_Usage;

begin
   Parse_Arguments (AP);

   if Arguments_Seen < 1 then
      Process_Standard_Input;
   end if;
end ABFromUnits;
