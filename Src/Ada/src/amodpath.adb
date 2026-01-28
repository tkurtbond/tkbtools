with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Containers;            use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;           use Ada.Directories;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Arg_Parser; use Arg_Parser;

procedure AModPath is
   Program_Name : String :=
     (if Command_Name = "" then "amodpath" else Command_Name);

   Exit_Program : exception;

   Debugging : constant Boolean := False;

   function Print_Version return Boolean is
   begin
      Put_Line (Program_Name & " version 0.1");
      OS_Exit (0);
      return True;
   end Print_Version;

   Warnings_Are_Fatal       : aliased Boolean          := True;
   No_Warnings              : aliased Boolean          := False;
   Directories_Are_Relative : aliased Boolean          := False;
   Exists_Flag              : aliased Boolean          := False;
   In_Path_Separator        : aliased Unbounded_String := Null_Unbounded_String;
   Out_Path_Separator       : aliased Unbounded_String := Null_Unbounded_String;

   procedure Debug (Message : String) is
   begin
      if Debugging then
         Put_Line (Standard_Error, Message);
         Flush (Standard_Error);
      end if;
   end Debug;

   procedure Warning (Message : String) is
   begin
      Put_Line (Standard_Error, Program_Name & ": warning: " & Message);
      Flush (Standard_Error);
   end Warning;

   procedure Warning_Or_Error (Message : String) is
      Message_Type : String :=
        (if Warnings_Are_Fatal then "error" else "warning");
   begin
      if not No_Warnings then
         Put (Standard_Error, Program_Name & ": " & Message_Type & ": ");
         Put_Line (Standard_Error, Message);
         Flush (Standard_Error);
         if Warnings_Are_Fatal then
            OS_Exit (1);
         end if;
      end if;
   end Warning_Or_Error;

   procedure Fatal_Error (Exit_Code : Natural; Message : String) is
   begin
      Put (Standard_Error, Program_Name & ": fatal error: ");
      Put_Line (Standard_Error, Message);
      Flush (Standard_Error);
      OS_Exit (Exit_Code);
   end Fatal_Error;

   function "+" (S : in String) return Unbounded_String renames
     To_Unbounded_String;

   function "+" (C : in Character) return String is (1 => C);

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
         --  The pattern can be longer than one character.
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
         String_Length := String_Length + Length (E);
      end loop;
      String_Length := String_Length + (Vector_Length - 1) * Separator'Length;
      declare
         Result : String (1 .. String_Length);
         I      : Natural  := 0;
         Start  : Positive := 1;
      begin
         for E of V loop
            declare
               E_Length : Integer := Length (E);
            begin
               I                                      := I + 1;
               Result (Start .. Start + E_Length - 1) := To_String (E);
               Start                                  := Start + E_Length;
               if I < Vector_Length then
                  Result (Start .. Start + Separator'Length - 1) := Separator;
                  Start := Start + Separator'Length;
               end if;
            end;
         end loop;
         return Result;
      end;
   end Join;

   Path_Variable : aliased Unbounded_String := +"PATH";
   Path_String   : aliased Unbounded_String := +Value (To_String (Path_Variable), "");
   Path_Vector   : Vector                   := Split (Path_String, +Path_Separator);

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

   function Get_Alternate_Path_Separator
     (Alternate_Path_Separator : Unbounded_String;
      Normal_Path_Separator    : String) return String is
     (if Length (Alternate_Path_Separator) = 0 then Normal_Path_Separator
      else To_String (Alternate_Path_Separator));

   type Output_Type is (Nice, Simple, Cmd, Csh, Sh, Quiet);
   Output : Output_Type := Sh;

   function Set_Path (Path : Unbounded_String) return Boolean is
      Separator : String :=
        Get_Alternate_Path_Separator
          (In_Path_Separator, String'(1 => Path_Separator));
   begin
      Path_String := Path;
      Path_Vector := Split (Path, Separator);
      return True;
   end Set_Path;

   function Set_Path_From_Variable
     (Variable : Unbounded_String) return Boolean
   is
      Path_Variable_String : String := To_String (Variable);
      Separator            : String :=
        Get_Alternate_Path_Separator
          (In_Path_Separator, String'(1 => Path_Separator));
   begin
      Path_String   := Null_Unbounded_String;
      Path_Vector   := Empty_Vector;
      begin
         Path_String := +Value (Path_Variable_String);
         Path_Vector := Split (Path_String, Separator);
      exception
         when Constraint_Error =>
            Warning_Or_Error
              ("unable to get path from environment variable " & Path_Variable_String);
      end;
      return True;
   end Set_Path_From_Variable;


   function  Set_Path_And_Variable_From_Variable
     (Variable : Unbounded_String) return Boolean
   is
      Path_Variable_String : String := To_String (Variable);
      Separator            : String :=
        Get_Alternate_Path_Separator
          (In_Path_Separator, String'(1 => Path_Separator));
   begin
      Path_Variable := Variable;
      Path_String   := Null_Unbounded_String;
      Path_Vector   := Empty_Vector;
      begin
         Path_String := +Value (Path_Variable_String);
         Path_Vector := Split (Path_String, Separator);
      exception
         when Constraint_Error =>
            Warning_Or_Error
              ("unable to get path from environment variable " &
                 Path_Variable_String);
      end;
      return True;
   end Set_Path_And_Variable_From_Variable;

   function Set_Add_After (Part : Unbounded_String) return Boolean is
   begin
      Todo := (After, Part);
      return True;
   end Set_Add_After;

   function Set_Add_Before (Part : Unbounded_String) return Boolean is
   begin
      Todo := (Before, Part);
      return True;
   end Set_Add_Before;

   function Set_Add_Start return Boolean is
   begin
      Todo := (Mode => Front);
      return True;
   end Set_Add_Start;

   function Set_Add_End return Boolean is
   begin
      Todo := (Mode => Back);
      return True;
   end Set_Add_End;

   procedure Add_End (Part : Unbounded_String) is
   begin
      Append (Path_Vector, Part);
   end Add_End;

   procedure Add_Start (Part : Unbounded_String) is
   begin
      Prepend (Path_Vector, Part);
   end Add_Start;

   function Unique return Boolean is
      New_Path_Vector : Vector := Empty_Vector;
   begin
      for E of Path_Vector loop
         if not Contains (New_Path_Vector, E) then
            Append (New_Path_Vector, E);
         end if;
      end loop;
      Path_Vector := New_Path_Vector;
      return True;
   end Unique;

   procedure Add_After (After, Part : Unbounded_String) is
      C : Cursor := Find (Path_Vector, After);
   begin
      if C = No_Element then
         Warning
           (To_String (After) & " is not in path to add " & To_String (Part) &
              " after it; adding at end");
         Append (Path_Vector, Part);
      else
         C := Next (C);
         Insert (Path_Vector, C, Part);
      end if;
   end Add_After;

   procedure Add_Before (Before, Part : Unbounded_String) is
      C : Cursor := Find (Path_Vector, Before);
   begin
      if C = No_Element then
         Warning
           (To_String (Before) & " is not in path to add " & To_String (Part) &
              " before it; adding at start");
         Prepend (Path_Vector, Part);
      else
         Insert (Path_Vector, C, Part);
      end if;
   end Add_Before;

   function Delete_Item (Part : Unbounded_String) return Boolean is
      C : Cursor := Find (Path_Vector, Part);
   begin
      if C = No_Element then
         Warning
           (To_String (Part) & " is not in path to delete it");
         return True;
      end if;
      loop
         Delete (Path_Vector, C);
         C := Find (Path_Vector, Part);
         exit when C = No_Element;
      end loop;
      return True;
   end Delete_Item;

   function Set_Separators (Separator : Unbounded_String) return Boolean is
   begin
      In_Path_Separator  := Separator;
      Out_Path_Separator := Separator;
      return True;
   end Set_Separators;

   function Make_Absolute (Part : String) return String is
   begin
      begin
         declare
            Item : String := Full_Name (Part);
         begin
            return Item;
         end;
      exception
         when Ada.Directories.Name_Error => -- The part is already absolute.
            return Part;
      end;
   end Make_Absolute;

   procedure Anonymous_Arg (Part : String) is
      Item : String :=
        (if Directories_Are_Relative then Part else Make_Absolute (Part));
   begin
      if not Exists_Flag or else Ada.Directories.Exists (Item) then
         case Todo.Mode is
            when After =>
               Add_After (Todo.Part, +Item);
            when Before =>
               Add_Before (Todo.Part, +Item);
            when Front =>
               Add_Start (+Item);
            when Back =>
               Add_End (+Item);
         end case;
      else
         Warning ("pathname does not exist: " & Part);
      end if;
      Exists_Flag := False;
      Todo        := (Mode => Back);
   end Anonymous_Arg;

   function Arg_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Anonymous_Arg (Arg);
      return True;
   end Arg_Handler;

   function Add_Current return Boolean is
      Current : String :=
        (if Directories_Are_Relative then Simple_Name (Current_Directory)
         else Current_Directory);
   begin
      Anonymous_Arg (Current);
      return True;
   end Add_Current;

   function Add_Empty return Boolean is
   begin
      Anonymous_Arg ("");
      return True;
   end Add_Empty;

   function Help return Boolean;

   function Set_Cmd_Output return Boolean is
   begin
      Output := Cmd;
      return True;
   end Set_Cmd_Output;

   function Set_Csh_Output return Boolean is
   begin
      Output := Csh;
      return True;
   end Set_Csh_Output;

   function Set_Msys_Output return Boolean is
   begin
      Fatal_Error (1, "MSYS style output is not yet implemented.");
      return True;
   end Set_Msys_Output;

   function Set_Nice_Output return Boolean is
   begin
      Output := Nice;
      return True;
   end Set_Nice_Output;

   function Set_Quiet_Output return Boolean is
   begin
      Output := Quiet;
      return True;
   end Set_Quiet_Output;

   function Set_Sh_Output return Boolean is
   begin
      Output := Sh;
      return True;
   end Set_Sh_Output;

   function Set_Simple_Output return Boolean is
   begin
      Output := Simple;
      return True;
   end Set_Simple_Output;

   Options : aliased Option_Array :=
     (Make_Set_Boolean_False_Option
       ("Non-absolute names are made absolute using the current directory.",
        'A', "absolute", Directories_Are_Relative'Unrestricted_Access),
       Make_Unbounded_String_Option
         ("Add next argument to the path after ARG.",
          'a', "after", Set_Add_After'Unrestricted_Access),
       Make_Unbounded_String_Option
         ("Add the next argument to the path before ARG.", 'b', "before", Set_Add_Before'Unrestricted_Access),
       Make_Option
         ("Output Windows cmd.exe to set path (not useful, alas).", 'D', "cmd", Set_Cmd_Output'Unrestricted_Access),
       Make_Option
         ("Output csh command to set the path.", 'C', "csh", Set_Csh_Output'Unrestricted_Access),
       Make_Option
         ("Add current directory to path.", 'c', "current", Add_Current'Unrestricted_Access),
       Make_Unbounded_String_Option
         ("Delete all occurances of ARG in path.", 'd', "delete", Delete_Item'Unrestricted_Access),
       Make_Option
         ("An an empty element to the path", 'E', "empty", Add_Empty'Unrestricted_Access),
       Make_Option
         ("Add next argument to the end of the path", 'e', "end", Set_Add_End'Unrestricted_Access),
       Make_Set_Boolean_True_Option
         ("Add next item only if it exists.", 'X', "exists", Exists_Flag'Unrestricted_Access),
       Make_Option
         ("Display the help message.", 'h', "help", Help'Unrestricted_Access),
       Make_Set_Unbounded_String_Option
         ("Set the input path separator.", 'i', "insep", In_Path_Separator'Unrestricted_Access),
       Make_Unbounded_String_Option
         ("Set the path from environment variable ARG.", 'I', "ivar", Set_Path_From_Variable'Unrestricted_Access),
       Make_Set_Boolean_True_Option
         ("Make all warnings fatal errors.", 'f', "fatal", Warnings_Are_Fatal'Unrestricted_Access),
       Make_Option
         ("Output path in msys format; WARNING: Not yet implemented!", 'M', "msys", Set_Msys_Output'Unrestricted_Access),
       Make_Set_Unbounded_String_Option
         ("Name of the path environment variable for output.", 'n', "name", Path_Variable'Unrestricted_Access),
       Make_Option
         ("Print the path out ""nicely"", one item per line.", 'N', "nice", Set_Nice_Output'Unrestricted_Access),
       Make_Set_Boolean_True_Option
         ("Don't output any warnings, and so don't error on warnings.", 'W', "no-warnings", No_Warnings'Unrestricted_Access),
       Make_Set_Unbounded_String_Option
         ("Set the output path separator.", 'o', "outsep", Out_Path_Separator'Unrestricted_Access),
       Make_Unbounded_String_Option
         ("Set the value of path to work on.", 'p', "path", Set_Path'Unrestricted_Access),
       Make_Option
         ("Don't print out the path.", 'Q', "quiet", Set_Quiet_Output'Unrestricted_Access),
       Make_Set_Boolean_True_Option
         ("Interpret non-absolute paths as relative to the current directory", 'R', "relative",
          Directories_Are_Relative'Unrestricted_Access),
       Make_Unbounded_String_Option
         ("Set the input and output path separators.", 'S', "sep", Set_Separators'Unrestricted_Access),
       Make_Option
         ("Output sh command to set the path.", 'U', "sh", Set_Sh_Output'Unrestricted_Access),
       Make_Option
         ("Simple output: just the new value.", 'P', "simple", Set_Simple_Output'Unrestricted_Access),
       Make_Option
         ("Add the next argument to the start of the path", 's', "start", Set_Add_Start'Unrestricted_Access),
       Make_Option
         ("Elimanate duplicate items AT THIS POINT IN PROCESSING", 'u', "unique", Unique'Unrestricted_Access),
       Make_Unbounded_String_Option
         ("Set the path from the environment variable ARG, and make ARG be the name of the output environment variable.",
          'v', "var", Set_Path_And_Variable_From_Variable'Unrestricted_Access),
       Make_Option
         ("Print the version number of this program.", 'V', "version", Print_Version'Unrestricted_Access),
       Make_Set_Boolean_False_Option
         ("Warn about missing environment variables instead of exiting with an error.",'w', "warnings",
          Warnings_Are_Fatal'Unrestricted_Access));

   AP : Argument_Parser :=
     Make_Argument_Parser (Program_Name & " [options] arguments...",
                           Arg_Handler'Unrestricted_Access, Options'Unrestricted_Access);

   function Help return Boolean is
      Time_To_End_It_All : exception;
   begin
      Usage (AP);
      -- Once they ask for help it is too late to continue.
      raise Exit_Program;
      return False;
   end Help;


begin
   --  loop
   --     exit when I >= Number_Of_Arguments;
   --     I := I + 1;
   --     declare
   --        Arg : String := Argument (I);
   --     begin
   --        if Arg = "--absolute" or Arg = "-A" then
   --           Directories_Are_Relative := False;
   --        elsif Arg = "--after" or Arg = "-a" then
   --           I := I + 1;
   --           Set_Add_After (+Argument (I));
   --        elsif Arg = "--before" or Arg = "-b" then
   --           I := I + 1;
   --           Set_Add_Before (+Argument (I));
   --        elsif Arg = "--cmd" or Arg = "-D" then
   --           -- DOS/Windows style, for cmd.exe.
   --           Output := Cmd;
   --        elsif Arg = "--csh" or Arg = "-C" then
   --           Output := Csh;
   --        elsif Arg = "--current" or Arg = "-c" then
   --           Add_Current;
   --        elsif Arg = "--delete" or Arg = "-d" then
   --           I := I + 1;
   --           Delete (+Argument (I));
   --        elsif Arg = "--empty" or Arg = "-E" then
   --           Add_Empty;
   --        elsif Arg = "--end" or Arg = "-e" then
   --           Set_Add_End;
   --        elsif Arg = "--exists" or Arg = "-X" then
   --           Exists_Flag := True;
   --        elsif Arg = "--insep" or Arg = "-i" then
   --           I := I + 1;
   --           In_Path_Separator := +Argument (I);
   --        elsif Arg = "--ivar" or Arg = "-I" then
   --           I := I + 1;
   --           Set_Path_From_Variable (+Argument (I));
   --        elsif Arg = "--fatal" or Arg = "-f" then
   --           Warnings_Are_Fatal := True;
   --        elsif Arg = "--msys" or Arg = "-M" then
   --           Fatal_Error (10, "--msys not yet implemented");
   --        elsif Arg = "--name" or Arg = "-n" then
   --           I := I + 1;
   --           Path_Variable := +Argument (I);
   --        elsif Arg = "--nice" or Arg = "-N" then
   --           Output := Nice;
   --        elsif Arg = "--no-warnings" or Arg = "-W" then
   --           -- Don't output any warnings, and so don't error on warnings;
   --           No_Warnings := True;
   --        elsif Arg = "--outsep" or Arg = "-o" then
   --           I := I + 1;
   --           Out_Path_Separator := +Argument (I);
   --        elsif Arg = "--path" or Arg = "-p" then
   --           I := I + 1;
   --           Set_Path (+Argument (I));
   --        elsif Arg = "--quiet" or Arg = "-Q" then
   --           Output := Quiet;
   --        elsif Arg = "--relative" or Arg = "-R" then
   --           Directories_Are_Relative := True;
   --        elsif Arg = "--sep" or Arg = "-S" then
   --           I := I + 1;
   --           Set_Separators (+Argument (I));
   --        elsif Arg = "--sh" or Arg = "-U" then
   --           Output := Sh;
   --        elsif Arg = "--simple" or Arg = "-P" then -- Plain
   --           Output := Simple;
   --        elsif Arg = "--start" or Arg = "-s" then
   --           Set_Add_Start;
   --        elsif Arg = "--unique" or Arg = "-u" then
   --           Unique;
   --        elsif Arg = "--var" or Arg = "-v" then
   --           I := I + 1;
   --           Set_Path_And_Variable_From_Variable (+Argument (I));
   --        elsif Arg = "--warnings" or Arg = "-w" then
   --           Warnings_Are_Fatal := False;
   --        elsif Arg = "--version" or Arg = "-V" then
   --           Print_Version;
   --        elsif Arg (1) = '-' then
   --           Fatal_Error (1, "unknown option """ & Arg & """.");
   --        else                           --  Bare argument, not option.
   --           Anonymous_Arg (Arg);
   --        end if;
   --     exception
   --        when Constraint_Error =>
   --           Fatal_Error (1, Program_Name & ": " & Arg & " missing parameter.");
   --     end;
   --  end loop;

   Parse_Arguments (AP);

   declare
      Separator : String :=
        Get_Alternate_Path_Separator
          (Out_Path_Separator, String'((1 => Path_Separator)));
   begin
      case Output is
         when Nice =>
            for E of Path_Vector loop
               Put_Line (To_String (E));
            end loop;
         when Simple =>
            declare
               Final_Path : String := "'" & Join (Path_Vector, Separator) & "'";
            begin
               Put_Line (Final_Path);
            end;
         when Cmd =>
            declare
               Separator  : String :=
                 Get_Alternate_Path_Separator (Out_Path_Separator, ":");
               Final_Path : String :=
                 "path '" & Join (Path_Vector, Separator) & "'";
            begin
               Put_Line (Final_Path);
            end;
         when Csh =>
            declare
               Final_Path : String :=
                 ("setenv " & To_String (Path_Variable) & " '" &
                  Join (Path_Vector, Separator)) & "'";
            begin
               Put_Line (Final_Path);
            end;
         when Sh =>
            declare
               Name       : String := To_String (Path_Variable);
               Final_Path : String :=
                 (Name & "='" & Join (Path_Vector, Separator) & "'");
            begin
               Put_Line (Final_Path);
               Put_Line ("export " & Name);
            end;
         when Quiet =>          -- Do nothing.
            null;
      end case;
   end;
end AModPath;
