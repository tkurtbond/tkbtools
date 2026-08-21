with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Arg_Parser;            use Arg_Parser;
with Ada.Calendar;          use Ada.Calendar;
with Ada.Calendar.Formatting;

procedure Anewver is

   package CF renames Ada.Calendar.Formatting;
   package S renames Ada.Strings;
   package SF renames Ada.Strings.Fixed;
   package D renames Ada.Directories;
   package DH renames Ada.Directories.Hierarchical_File_Names;
   package TIO renames Ada.Text_IO;

   Output_Directory  : String_Reference;
   Label             : String_Reference;
   Dry_Run           : aliased Boolean := False;
   Separator         : String_Reference;
   Separator_Default : String          := "_";

   function Do_Help return Boolean; -- Forward declaration.

   Options : aliased Option_Array :=
     (Make_Set_String_Option
        (Description => "Set output directory.", Short_Name => 'd', Long_Name => "output-directory",
         Variable    => Output_Directory'Unrestricted_Access),
      Make_Option
        (Description => "Print a help message.", Short_Name => 'h', Long_Name => "help", Handler => Do_Help'Unrestricted_Access),

      Make_Set_String_Option
        (Description => "Label to add after the data", Short_Name => 'l', Long_Name => "label",
         Variable    => Label'Unrestricted_Access),

      Make_Set_Boolean_True_Option
        (Description => "Don't Actually do Anything.", Short_Name => 'n', Long_Name => "dry-run",
         Variable    => Dry_Run'Unrestricted_Access),

      Make_Set_String_Option
        (Description => "Separator between added parts.", Short_Name => 's', Long_Name => "separator",
         Variable    => Separator'Unrestricted_Access));

   procedure Process_Path_Name (Path_Name : String) is
      function Numeric_Part (N : Integer) return String is
      begin
         if N = 0 then
            return "";
         else
            return SF.Trim (N'Image, S.Left);
         end if;
      end Numeric_Part;

      Separator      : String := (if Anewver.Separator /= null then Anewver.Separator.all else Separator_Default);
      Extension      : String := (if D.Extension (Path_Name) = "" then "" else "." & D.Extension (Path_Name));
      Directory_Name : String := D.Containing_Directory (Path_Name);
      File_Name      : String := D.Simple_Name (Path_Name);
      Base_Name      : String := D.Base_Name (Path_Name);

      Now         : Time   := Clock;
      Date_String : String := CF.Local_Image (Now) (1 .. 10);

      I : Integer := 0;
   begin
      loop
         declare
            New_Name : String :=
              DH.Compose
                ((if Output_Directory /= null then Output_Directory.all else Directory_Name),
                 Base_Name & Separator & Date_String & (if Label /= null then Separator & Label.all else "") & Numeric_Part (I),
                 Extension);
         begin
            if D.Exists (New_Name) then
               I := I + 1;
            else
               TIO.Put_Line
                 ("'" & Path_Name & "'" & (if Dry_Run then " would be copied to " else " copied to ") & "'" & New_Name & "'");
               if not Dry_Run then
                  D.Copy_File (Path_Name, New_Name);
               end if;
               exit;
            end if;
         end;
      end loop;
   end Process_Path_Name;

   function Arg_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Process_Path_Name (Arg);
      return True;
   end Arg_Handler;

   Usage_Description : constant String :=
     "anewver [options...] files..." & ASCII.LF & ASCII.LF &
     "anewver makes copies of files, with file.ext being copied to file-YYYY-MM-DD.ext " & ASCII.LF &
     "if that file doesn't exist,  or with _<N> appended after the date if that file exists, " & ASCII.LF &
     "with N starting at 1 and increasing until no file with that name already exists.";

   AP : Argument_Parser := Make_Argument_Parser (Usage_Description, Arg_Handler'Unrestricted_Access, Options'Unrestricted_Access);

   function Do_Help return Boolean is
      End_Program : exception;
   begin
      Arg_Parser.Usage (AP);
      --  Once they ask for help it is too late to continue.
      raise End_Program;
      return False;
   end Do_Help;

begin
   Parse_Arguments (AP);
end Anewver;
