with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals; use Ada.Numerics.Big_Numbers.Big_Reals;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Arg_Parser; use Arg_Parser;

procedure ABToUnits is
   Program_Name : String :=
     (if Command_Name = "" then "atounits" else Command_Name);

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
            Put (Head (+E.SI_Label, 6) & " (" & (+E.SI_Abbreviation) & ")  " & (+E.Si_Text) & " ");
            Put (Trim (To_String (E.SI, Aft => 1), Both));
            New_Line;
         end;
      end loop;
      New_Line;

      Put_Line ("Binary Prefixes: https://en.wikipedia.org/wiki/Binary_prefix");
      for C in Multipliers'Range loop
         declare
            E: Labeled_Multiplier renames Multipliers (C);
         begin
            Put (Head (+E.BI_Label, 6) & " (" & (+E.BI_Abbreviation) & ") " & (+E.BI_Text) & " ");
            Put (Trim (To_String (E.BI, Aft => 1), Both));
            New_Line;
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

   Mult_By_K: aliased Boolean := False;
   Mult_By_M: aliased Boolean := False;
   Mult_By_G: aliased Boolean := False;
   Mult_By_T: aliased Boolean := False;
   Mult_By_P: aliased Boolean := False;
   Mult_By_E: aliased Boolean := False;
   Mult_By_Z: aliased Boolean := False;
   Mult_By_Y: aliased Boolean := False;
   Mult_By_R: aliased Boolean := False;
   Mult_By_Q: aliased Boolean := False;

   Units : Unbounded_String := +"B"; -- Units default to bytes.

   Arguments_Seen : Natural := 0;

   function Process_Argument (Start_With : Positive; Arg: String) return Boolean is
   begin
      Arguments_Seen := Arguments_Seen + 1;
      declare
         R_In: Big_Real := Relaxed_From_String (Arg);
         R_Out : Big_Real := R_In;
         type Found_Multiplier (Found : Boolean := False) is
            record
               case Found is
                  when True => Multiplier_Used : Multiplier;
                  when False => null;
               end case;
            end record;

         Multiplier_Used : Found_Multiplier;
      begin
         if Mult_By_K then R_Out := R_Out * By_Multiplier ('K'); end if;
         if Mult_By_M then R_Out := R_Out * By_Multiplier ('M'); end if;
         if Mult_By_G then R_Out := R_Out * By_Multiplier ('G'); end if;
         if Mult_By_T then R_Out := R_Out * By_Multiplier ('T'); end if;
         if Mult_By_P then R_Out := R_Out * By_Multiplier ('P'); end if;
         if Mult_By_E then R_Out := R_Out * By_Multiplier ('E'); end if;
         if Mult_By_Z then R_Out := R_Out * By_Multiplier ('Z'); end if;
         if Mult_By_Y then R_Out := R_Out * By_Multiplier ('Y'); end if;
         if Mult_By_R then R_Out := R_Out * By_Multiplier ('R'); end if;
         if Mult_By_Q then R_Out := R_Out * By_Multiplier ('Q'); end if;

         if R_Out >= By_Multiplier ('Q') then
            R_Out := R_out / By_Multiplier ('Q');
            Multiplier_Used := (True, 'Q');
         elsif R_Out >= By_Multiplier ('R') then
            R_Out := R_Out / By_Multiplier ('R');
            Multiplier_Used := (True, 'R');
         elsif R_Out >= By_Multiplier ('Y') then
            R_Out := R_Out / By_Multiplier ('Y');
            Multiplier_Used := (True, 'Y');
         elsif R_Out >= By_Multiplier ('Z') then
            R_Out := R_Out / By_Multiplier ('Z');
            Multiplier_Used := (True, 'Z');
         elsif R_Out >= By_Multiplier ('E') then
            R_Out := R_Out / By_Multiplier ('E');
            Multiplier_Used := (True, 'E');
         elsif R_Out >= By_Multiplier ('P') then
            R_Out := R_Out / By_Multiplier ('P');
            Multiplier_Used := (True, 'P');
         elsif R_Out >= By_Multiplier ('T') then
            R_Out := R_Out / By_Multiplier ('T');
            Multiplier_Used := (True, 'T');
         elsif R_Out >= By_Multiplier ('G') then
            R_Out := R_Out / By_Multiplier ('G');
            Multiplier_Used := (True, 'G');
         elsif R_Out >= By_Multiplier ('M') then
            R_Out := R_Out / By_Multiplier ('M');
            Multiplier_Used := (True, 'M');
         elsif R_Out >= By_Multiplier ('K') then
            R_Out := R_Out / By_Multiplier ('K');
            Multiplier_Used := (True, 'K');
         else
            Multiplier_Used := (Found => False);
         end if;

         Put (Trim (To_String (R_Out), Both));
         if Multiplier_Used.Found then
            Put (+Abbreviation (Multiplier_Used.Multiplier_Used) & (+Units));
         end if;
         New_Line;
      end;
      return True;
   exception
      when CONSTRAINT_ERROR =>
         -- Raised in Relaxed_From_String, above in this function.
         null;
         return True;
   end Process_Argument;

   procedure Process_Standard_Input is
      Continue : Boolean := True;
   begin
      loop
         exit when End_Of_File or Continue;
         declare
            S : String := Trim (Get_Line, Both);
         begin
            Continue := Process_Argument (1, S);
         end;
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
         ("Set the units to use.", 'u', "units", Units'Unrestricted_access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Kilo",   'K', "kilo",   Mult_By_K'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Mega",   'M', "mega",   Mult_By_M'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Giga",   'G', "giga",   Mult_By_G'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Tera",   'T', "tera",   Mult_By_T'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Peta",   'P', "peta",   Mult_By_P'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Exa",    'E', "exa",    Mult_By_E'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Zeta",   'Z', "zeta",   Mult_By_Z'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Yotta",  'Y', "yotta",  Mult_By_Y'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Ronna",  'R', "ronna",  Mult_By_R'Unrestricted_Access),
       Make_Set_Boolean_True_Option("Multiply the result by the appropriate Quetta", 'Q', "quetta", Mult_By_Q'Unrestricted_Access));

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
end ABToUnits;
