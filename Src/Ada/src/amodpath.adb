with Ada.Containers;            use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;           use Ada.Directories;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

procedure AModPath is
   Program_Name       : String  := Command_Name;
   
   procedure Print_Version is 
   begin
      Put_Line (Program_Name & " version 0.1");
      Os_Exit (0);
   end Print_Version;

   Warnings_Are_Fatal : Boolean := False;
   Relative_Flag : Boolean := False;
   Exists_Flag : Boolean := False;
   In_Path_Separator : Unbounded_String := Null_Unbounded_String;
   Out_Path_Separator : Unbounded_String := Null_Unbounded_String;

   procedure Debug (Message : String) is
   begin
      if True then
         Put_Line (Standard_Error, Message);
         Flush (Standard_Error);
      end if;
   end Debug;

   procedure Warning (Message : String) is
      Message_Type : String := 
        (if Warnings_Are_Fatal then "error" else "warning");
   begin
      Put (Standard_Error, Program_Name & ": " & Message_Type & ": ");
      Put_Line (Standard_Error, Message);
      Flush (Standard_Error);
      if Warnings_Are_Fatal then
         OS_Exit (1);
      end if;
   end Warning;

   procedure Error (Message : String) is
   begin
      Put (Standard_Error, Program_Name & ": error: ");
      Put_Line (Standard_Error, Message);
      Flush (Standard_Error);
   end Error;

   procedure Fatal_Error (Exit_Code : Natural; Message : String) is
   begin
      Put (Standard_Error, Program_Name & ": fatal error: ");
      Put_Line (Standard_Error, Message);
      Flush (Standard_Error);
      OS_Exit (Exit_Code);
   end Fatal_Error;

   function "+" (S : in String) return Unbounded_String renames
     To_Unbounded_String;

   function "+" (C : in Character) return String is ([C]);

   package Unbounded_String_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   use Unbounded_String_Vectors;

   function Split (S : Unbounded_String; Pattern : String) return Vector is
      Start     : Positive := 1;
      Position  : Natural;
      Num_Parts : Natural  := 0;
      V         : Vector;
   begin
      while Start <= Length (S) loop
         Position := Index (S, Pattern, Start);
         exit when Position = 0;
         Append (V, Unbounded_Slice (S, Start, Position - 1));
         -- The pattern can be longer than one character.
         Start := Position + Pattern'Length;
      end loop;
      Num_Parts := Num_Parts + 1;
      Append (V, Unbounded_Slice (S, Start, Length (S)));

      return V;
   end Split;

   function Join (V : Vector; Separator : String) return String is
      Vector_Length : Integer := Integer (Length (V));
      String_Length : Integer := 0;
   begin
      for E of V loop
         String_Length := @ + Length (E);
      end loop;
      String_Length := @ + (Vector_Length - 1) * Separator'Length;
      declare
         Result : String (1 .. String_Length);
         I      : Natural  := 0;
         Start  : Positive := 1;
      begin
         Debug
           ("Result'First: " & Result'First'Image & " Result'Last: " &
            Result'Last'Image);
         for E of V loop
            declare
               E_Length : Integer := Length (E);
            begin
               I := @ + 1;
               Debug
                 ("Start: " & Start'Image & " Start+E_Length-1: " &
                  Integer'Image (Start + E_Length - 1));
               Result (Start .. Start + E_Length - 1) := To_String (E);
               Start                                  := @ + E_Length;
               if I > 1 and I < Vector_Length then
                  Result (Start .. Start + Separator'Length - 1) := Separator;
                  Start := @ + Separator'Length;
               end if;
            end;
         end loop;
         return Result;
      end;
   end Join;

   Path_Variable : Unbounded_String := +"PATH";
   Path_String : Unbounded_String := +Value (To_String (Path_Variable), "");
   Path_Vector : Vector           := Split (Path_String, +Path_Separator);

   type Todo_Mode is (After, Before, Front, Back);
   type Todo_Type (Mode : Todo_Mode := Back) is record
      case Mode is
         when After | Before =>
            Part : Unbounded_String;
         when Front | Back =>
            null;
      end case;
   end record;

   Todo : Todo_Type;

   procedure Check_Todo (Todo : Todo_Type) is
   begin
      case Todo.Mode is
         when After =>
            null;
         when Before =>
            null;
         when Front =>
            null;
         when Back =>
            null;
      end case;
   end Check_Todo;

   type Output_Type is (Nice, Simple, Cmd, Csh, Sh, Quiet);
   Output : Output_Type := Sh;

   procedure Set_Path (Path : Unbounded_String) is
   begin
      Path_String := Path;
      Path_Vector := Split (Path, +Path_Separator);
   end Set_Path;

   procedure Set_Path_From_Variable (Variable : Unbounded_String) is
      Path_Variable_String : String := To_String (Variable);
   begin
      Path_Variable := Variable;
      Path_String := Null_Unbounded_String;
      Path_String := +Value (Path_Variable_String);
   exception
      when Constraint_Error =>
         Warning ("unable to set path from " & Path_Variable_String & ".");
   end Set_Path_From_Variable;

   procedure Set_Add_After (Part : Unbounded_String) is 
   begin 
      Todo := (After, Part);
   end Set_Add_After;

   procedure Set_Add_Before (Part : Unbounded_String) is 
   begin
      Todo := (Before, Part);
   end Set_Add_Before;

   procedure Set_Add_Start is 
   begin 
      Todo := (Mode => Front);
   end Set_Add_Start;

   procedure Set_Add_End is 
   begin
      Todo := (Mode => Back);
   end Set_Add_End;
   
   procedure Add_End (Part: Unbounded_String) is 
   begin
      Append (Path_Vector, Part);
   end Add_End;
   
   procedure Add_Start (Part: Unbounded_String) is 
   begin
      Prepend (Path_Vector, Part);
   end Add_Start;
   
   procedure Unique is
      New_Path_Vector : Vector := Empty_Vector;
   begin
      for E of Path_Vector loop
         if not Contains (New_Path_Vector, E) then 
            Append (New_Path_Vector, E);
         end if;
      end loop;
      Path_Vector := New_Path_Vector;
   end Unique;
   
   procedure Add_After (After, Part: Unbounded_String) is 
      C : Cursor := Find (Path_Vector, After);
   begin
      if C = No_Element then 
         Warning ("unable to find """ & To_String (After) & """ to insert """ &
                    To_String (Part) & """ after it.");
      else
         C := Next (C);
         Path_Vector.Insert (C, Part);
      end if;
   end Add_After;
   
   procedure Add_Before (Before, Part: Unbounded_String) is 
      C : Cursor := Find (Path_Vector, Before);
   begin
      if C = No_Element then 
         Warning ("unable to find """ & To_String (Before) & """ to insert """ &
                    To_String (Part) & """ before it.");
      else
         Path_Vector.Insert (C, Part);
      end if;
   end Add_Before;
   
   procedure Delete (Part: Unbounded_String) is 
      C : Cursor;
   begin
      loop 
         C := Find (Path_Vector, Part);
         exit when C /= No_Element;
         Delete (Path_Vector, C);
      end loop;
   end Delete;
   
   procedure Set_Separators (Separator: Unbounded_String) is 
   begin
      In_Path_Separator := Separator;
      Out_Path_Separator := Separator;
   end Set_Separators;
   
   procedure Anonymous_Arg (Part: String) is 
      Item: String := 
        (if Relative_Flag then part 
          else Compose (Current_Directory, Part));
   begin
      if not Exists_Flag or else Ada.Directories.Exists (Item) then 
         case Todo.Mode is 
            when After => Add_After (Todo.Part, +Item);
            when Before => Add_Before (Todo.Part, +Item);
            when Front => Add_Start (+Item);
            when Back => Add_End (+Item);
         end case;
      else 
         Warning ("pathname """ & Item & """ does not exist.");
      end if;
      Exists_Flag := False;
      Todo := (Mode => Back);
   end Anonymous_Arg;
   
   procedure Add_Current is 
   begin
      Anonymous_Arg (Current_Directory);
   end Add_Current;
   
   procedure Add_Empty is 
   begin
      Anonymous_Arg ("");
   end Add_Empty;

   Number_Of_Arguments : Natural := Argument_Count;
   I                   : Natural := 0;

begin
   loop
      exit when I >= Number_Of_Arguments;
      I := @ + 1;
      declare
         Arg : String := Argument (I);
      begin
         if Arg = "--after" or Arg = "-a" then
            I    := @ + 1;
            Set_Add_After (+Argument (I));
         elsif Arg = "--before" or Arg = "-b" then
            I := @ + 1;
            Set_Add_Before (+Argument (I));
         elsif Arg = "--cmd" then
            Output := Cmd;
         elsif Arg = "--csh" then
            Output := Cmd;
         elsif Arg = "--current" then
            Add_Current;
         elsif Arg = "--delete" or Arg = "-d" then 
            I := @ + 1;
            Delete (+Argument (I));
         elsif Arg = "--empty" then
            Add_Empty;
         elsif Arg = "--end" or Arg = "-e" then
            Set_Add_End;
         elsif Arg = "--exists" then 
            Exists_Flag := True;
         elsif Arg = "--insep" then
            I := @ + 1;
            In_Path_Separator := +Argument (I);
         elsif Arg = "-outsep" then
            I := @ + 1;
            Out_Path_Separator := +Argument (I);
         elsif Arg = "--msys" then 
            Fatal_Error (10, "--msys not yet implemented");
         elsif Arg = "--name" or Arg = "-n" then
            I := I + 1;
            Path_Variable := +Argument (I);
         elsif Arg = "--nice" then
            Output := Nice;
         elsif Arg = "--path" or Arg = "-p" then 
            I := @ + 1;
            Set_Path (+Argument (I));
         elsif Arg = "--quiet" then 
            Output := Quiet;
         elsif Arg = "--relative" then
            Relative_Flag := True;
         elsif Arg = "--sep" then 
            I := @ + 1;
            Set_Separators (+Argument (I));
         elsif Arg = "--sh" then 
            Output := Sh;
         elsif Arg = "--simple" then
            Output := Simple;
         elsif Arg = "--start" or Arg = "-s" then 
            Set_Add_Start;
         elsif Arg = "--unique" then 
            Unique;
         elsif Arg = "--var" or Arg = "-v" then 
            I := @ + 1;
            Set_Path_From_Variable (+Argument (I));
         elsif Arg = "--warn" then 
            Warnings_Are_Fatal := False;
         elsif Arg = "--version" then 
            Print_Version;
         else                           --  Bare argument, not option.
            case Todo.Mode is
               when After =>
                  Set_Add_After (+Arg);
               when Before =>
                  Set_Add_Before (+Arg);
               when Front =>
                  Set_Add_Start;
               when Back =>
                  Set_Add_End;
            end case;
            Todo := (Mode => Back);
         end if;
      exception
         when Constraint_Error =>
            Fatal_Error (1, Program_Name & ": " & Arg & " missing parameter.");
      end;
   end loop;

   Put_Line ("PATH: " & Join (Path_Vector, +Path_Separator));

end AModPath;
