-- I originally thought to use Ada.Directories with this, but it was just simpler to do it manually.
-- This agrees with my basename and chopext.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Fixed;

procedure Achopext is
   package CL renames Ada.Command_Line;
   package S renames Ada.Strings;
   package SF renames Ada.Strings.Fixed;
begin
   if CL.Argument_Count /= 2 then
      Put_Line (Current_Error, CL.Command_Name & ": usage: " & CL.Command_Name & " FILENAME EXTENSION");
      CL.Set_Exit_Status (1);
      return;
   end if;
   declare
      Filename : String := CL.Argument (1);
      Extension : String := CL.Argument (2);
   begin
      if Filename'Length > Extension'Length and then Extension = Filename(Filename'Last - (Extension'Length - 1)..Filename'Last) then
         Put_Line (Filename(1..Filename'Last - Extension'Length));
      else
         Put_Line (Filename);
      end if;
   end;
end Achopext;
