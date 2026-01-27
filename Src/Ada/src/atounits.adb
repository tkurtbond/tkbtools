with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Long_Float_Text_IO; use Ada.Long_Long_Float_Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure AToUnits is
   Program_Name : String :=
     (if Command_Name = "" then "atounits" else Command_Name);

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
      SI : Long_Long_Float;
      BI_Label : Unbounded_String;
      BI_Abbreviation : Unbounded_String;
      BI : Long_Long_Float;
   end record;

   type Labeled_Multiplier_Array is array (Multiplier range <>)
     of Labeled_Multiplier;

   Use_SI : Boolean := False;

   Multipliers : Labeled_Multiplier_Array :=
     ['K' => (+"Kilo",   +"K", 10.0**03, +"Kibi", +"Ki", 2.0**010),
      'M' => (+"Mega",   +"M", 10.0**06, +"Mebi", +"Mi", 2.0**020),
      'G' => (+"Giga",   +"G", 10.0**09, +"Gibi", +"Gi", 2.0**030),
      'T' => (+"Tera",   +"T", 10.0**12, +"Tebi", +"Ti", 2.0**040),
      'P' => (+"Peta",   +"P", 10.0**15, +"Pebi", +"Pi", 2.0**050),
      'E' => (+"Exa",    +"E", 10.0**18, +"Exbi", +"Ei", 2.0**060),
      'Z' => (+"Zeta",   +"Z", 10.0**21, +"Zebi", +"Zi", 2.0**070),
      'Y' => (+"Yotta",  +"Y", 10.0**24, +"Yobi", +"Yi", 2.0**080),
      'R' => (+"Ronna",  +"R", 10.0**27, +"Robi", +"Ri", 2.0**090),
      'Q' => (+"Quetta", +"Q", 10.0**30, +"Qubi", +"Qi", 2.0**100)
     ];

   function By_Multiplier (M : Multiplier) return Long_Long_Float is
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
            Put (Head (+E.BI_Label & ": ", 8));
            Put (E.BI, Aft => 1, Exp => 0);
            New_Line;
         end;
      end loop;
   end Print_Binary_Prefixes;

   procedure Print_SI_Prefixes is
   begin
      for C in Multipliers'Range loop
         declare
            E : Labeled_Multiplier renames Multipliers (C);
         begin
            Put (Head (+E.SI_Label & ": ", 8));
            Put (E.SI, Aft => 1, Exp => 0);
            New_Line;
         end;
      end loop;
   end Print_SI_Prefixes;

   function Relaxed_From_String (S: String) return Long_Long_Float is
   begin
      declare
         R: Long_Long_Float := Long_Long_Float'Value (S);
      begin
         return R;
      end;
   exception
      when CONSTRAINT_ERROR =>
         Error ("The value """ & S & """ is not a valid floating point number.");
         raise;
   end Relaxed_From_String;

   Mult_By_K: Boolean := False;
   Mult_By_M: Boolean := False;
   Mult_By_G: Boolean := False;
   Mult_By_T: Boolean := False;
   Mult_By_P: Boolean := False;
   Mult_By_E: Boolean := False;
   Mult_By_Z: Boolean := False;
   Mult_By_Y: Boolean := False;
   Mult_By_R: Boolean := False;
   Mult_By_Q: Boolean := False;

   Units : Unbounded_String := +"B"; -- Units default to bytes.

   Number_Of_Arguments : Natural := 0;

   procedure Process_Argument (S: String) is
   begin
      declare
         R_In: Long_Long_Float := Relaxed_From_String (S);
         R_Out : Long_Long_Float := R_In;
         type Found_Multiplier (Found : Boolean := False) is
            record
               case Found is
                  when True => Multiplier_Used : Multiplier;
                  when False => null;
               end case;
            end record;

         Multiplier_Used : Found_Multiplier;
      begin
         if Mult_By_K then R_Out := @ * By_Multiplier ('K'); end if;
         if Mult_By_M then R_Out := @ * By_Multiplier ('M'); end if;
         if Mult_By_G then R_Out := @ * By_Multiplier ('G'); end if;
         if Mult_By_T then R_Out := @ * By_Multiplier ('T'); end if;
         if Mult_By_P then R_Out := @ * By_Multiplier ('P'); end if;
         if Mult_By_E then R_Out := @ * By_Multiplier ('E'); end if;
         if Mult_By_Z then R_Out := @ * By_Multiplier ('Z'); end if;
         if Mult_By_Y then R_Out := @ * By_Multiplier ('Y'); end if;
         if Mult_By_R then R_Out := @ * By_Multiplier ('R'); end if;
         if Mult_By_Q then R_Out := @ * By_Multiplier ('Q'); end if;

         if R_Out >= By_Multiplier ('Q') then
            R_Out := @ / By_Multiplier ('Q');
            Multiplier_Used := (True, 'Q');
         elsif R_Out >= By_Multiplier ('R') then
            R_Out := @ / By_Multiplier ('R');
            Multiplier_Used := (True, 'R');
         elsif R_Out >= By_Multiplier ('Y') then
            R_Out := @ / By_Multiplier ('Y');
            Multiplier_Used := (True, 'Y');
         elsif R_Out >= By_Multiplier ('Z') then
            R_Out := @ / By_Multiplier ('Z');
            Multiplier_Used := (True, 'Z');
         elsif R_Out >= By_Multiplier ('E') then
            R_Out := @ / By_Multiplier ('E');
            Multiplier_Used := (True, 'E');
         elsif R_Out >= By_Multiplier ('P') then
            R_Out := @ / By_Multiplier ('P');
            Multiplier_Used := (True, 'P');
         elsif R_Out >= By_Multiplier ('T') then
            R_Out := @ / By_Multiplier ('T');
            Multiplier_Used := (True, 'T');
         elsif R_Out >= By_Multiplier ('G') then
            R_Out := @ / By_Multiplier ('G');
            Multiplier_Used := (True, 'G');
         elsif R_Out >= By_Multiplier ('M') then
            R_Out := @ / By_Multiplier ('M');
            Multiplier_Used := (True, 'M');
         elsif R_Out >= By_Multiplier ('K') then
            R_Out := @ / By_Multiplier ('K');
            Multiplier_Used := (True, 'K');
         else
            Multiplier_Used := (Found => False);
         end if;

         Put (R_Out, Aft => 1, Exp => 0);
         if Multiplier_Used.Found then
            Put (+Abbreviation (Multiplier_Used.Multiplier_Used) & (+Units));
         end if;
         New_Line;
      end;
   exception
      when CONSTRAINT_ERROR =>
         -- Raised in Relaxed_From_String, above in this function.
         null;
   end Process_Argument;

   procedure Process_Standard_Input is
   begin
      loop
         exit when End_Of_File;
         declare
            S : String := Trim (Get_Line, Both);
         begin
            Process_Argument (S);
         end;
      end loop;
   end Process_Standard_Input;

   procedure Print_Usage is
   begin
      Put_Line ("usage: " & Program_Name & " [option ...] [value ...]");
      New_Line;
   end Print_Usage;

begin
   loop
      declare
         Ch: Character := Getopt ("B b h S s u: K M G T P E Z Y R Q k m g t p e z y r q");
      begin
         case Ch is
            when ASCII.NUL => exit;
            when 'B' => Print_Binary_Prefixes;
            when 'b' => Use_SI := False;
            when 'h' => Print_Usage; GNAT.OS_Lib.OS_Exit (1);
            when 'S' => Print_Si_Prefixes;
            when 's' => Use_SI := True;
            when 'u' => Units := +Parameter;
            when 'K' | 'k' => Mult_By_K := True;
            when 'M' | 'm' => Mult_By_M := True;
            when 'G' | 'g' => Mult_By_G := True;
            when 'T' | 't' => Mult_By_T := True;
            when 'P' | 'p' => Mult_By_P := True;
            when 'E' | 'e' => Mult_By_E := True;
            when 'Z' | 'z' => Mult_By_Z := True;
            when 'Y' | 'y' => Mult_By_Y := True;
            when 'R' | 'r' => Mult_By_R := True;
            when 'Q' | 'q' => Mult_By_Q := True;
            when others =>
               Fatal_Error (1, "unhandled option -" & Ch);
         end case;
      end;
   end loop;

   loop
      declare
         Arg : constant String := Trim (Get_Argument, Both);
      begin
         exit when Arg'Length = 0;
         Number_Of_Arguments := @ + 1;
         Process_Argument (Arg);
      end;
   end loop;

   if Number_Of_Arguments < 1 then
      Process_Standard_Input;
   end if;
end AToUnits;
