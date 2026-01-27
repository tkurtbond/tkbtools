with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Long_Float_Text_IO; use Ada.Long_Long_Float_Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure AFromUnits is
   Program_Name : String :=
     (if Command_Name = "" then "affromunits" else Command_Name);

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
      SI : Long_Long_Float ;
      BI_Label : Unbounded_String;
      BI_Abbreviation : Unbounded_String;
      BI_Text : Unbounded_String;
      BI : Long_Long_Float;
   end record;

   type Labeled_Multiplier_Array is array (Multiplier range <>)
     of Labeled_Multiplier;

   Use_SI : Boolean := False;

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

   function By_Multiplier (M : Multiplier) return Long_Long_Float is
     (if Use_SI then Multipliers (M).SI else Multipliers (M).BI);

   function Abbreviation (M: Multiplier) return Unbounded_String is
     (if Use_SI then Multipliers (M).SI_Abbreviation
      else Multipliers (M).BI_Abbreviation);

   procedure Maybe_Multiply (R : in out Long_Long_Float; S: String) is
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

   procedure Print_Prefixes is
   begin
      Put_Line ("SI/Metric Prefixes: https://en.wikipedia.org/wiki/Metric_prefix");
      for C in Multipliers'Range loop
         declare
            E: Labeled_Multiplier renames Multipliers (C);
         begin
            Put (Head (+E.SI_Label, 6) & " (" & (+E.SI_Abbreviation) & ")  " & (+E.Si_Text) & " ");
            Put (E.SI, Aft => 1, Exp => 0);
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
            Put (E.BI, Aft => 1, Exp => 0);
            New_Line;
         end;
      end loop;

   end Print_Prefixes;

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

   Units : Unbounded_String := +"B"; -- Units default to bytes.

   Number_Of_Arguments : Natural := 0;

   procedure Process_Argument (Arg: String) is
      S : String := Trim (Arg, Both);
      F : constant Integer := S'First;
      L : constant Integer := S'Last;
      I : Integer := S'First;
      R : Long_Long_Float;
   begin
      -- Ugh.  This is so ugly.
      while I <= L and then (Is_Digit (S(I))
                               or else S(I) = '_' or else S(I) = '.'
                               or else S(I) = '+' or else S(I) = '-')
      loop
         I := I + 1;
      end loop;

      R := Relaxed_From_String (S(F..I - 1));
      if I <= L then
         Maybe_Multiply (R, S(I..I));
      end if;
      Put (R, Aft => 1, Exp => 0);
      New_Line;
   end Process_Argument;

   procedure Process_Standard_Input is
   begin
      loop
         exit when End_Of_File;
         Process_Argument (Get_Line);
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
         Ch: Character := Getopt ("- b h p s u:");
      begin
         case Ch is
            when ASCII.NUL | '-' => exit;
            when 'b' => Use_SI := False;
            when 'h' => Print_Usage; GNAT.OS_Lib.OS_Exit (1);
            when 'p' => Print_Prefixes;
            when 's' => Use_SI := True;
            when 'u' => Units := +Parameter;
            when others =>
               Fatal_Error (1, "unhandled option -" & Ch);
         end case;
      end;
   end loop;

   loop
      declare
         Arg : constant String := Get_Argument;
      begin
         exit when Arg'Length = 0;
         Number_Of_Arguments := Number_Of_Arguments + 1;
         Process_Argument (Arg);
      end;
   end loop;

   if Number_Of_Arguments < 1 then
      Process_Standard_Input;
   end if;
end AFromUnits;
