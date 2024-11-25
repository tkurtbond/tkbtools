with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure AModPath is 
   Number_Of_Arguments : Natural := Argument_Count;
   Program_Name : String := Command_Name;
   I : Natural := 0;

   function "+"(S : in String) return Unbounded_String 
     renames To_Unbounded_String; 

   procedure Debug (Message: String) is 
   begin
      if False then 
         Put_Line (Standard_Error, Message);
         Flush (Standard_Error);
      end if;
   end Debug;

   procedure Error (Exit_Code: Natural; Message: String) is 
   begin
      Put_Line (Standard_Error, Message);
      Flush (Standard_Error);
      OS_Exit (Exit_Code);
   end Error;

   procedure Set_Add_After_Mode (Item: String) is 
   begin
      Put_Line (Standard_Error, "set_add_after_mode: " & Item);
   end Set_Add_After_Mode;
begin
   loop 
      exit when I >= Number_Of_Arguments;
      I := @ + 1;
      declare 
         Arg : String := Argument (I);
      begin
         if Arg = "--after" or Arg =  "-a" then 
            I := @ + 1;
            Set_Add_After_Mode (Argument (I));
         end if;
      exception
         when CONSTRAINT_ERROR => 
            Error (1, Program_Name & ": " & Arg & " missing argument.");
      end;
   end loop;
end AModPath;
