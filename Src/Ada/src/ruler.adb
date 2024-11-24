with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with GNAT.OS_LIb;
with GNAT.Formatted_String; use GNAT.Formatted_String;
procedure Ruler is 
   
   Ruler_Length : Positive := 80;
   Number_Of_Parts : Positive;

   procedure Usage is 
   begin
      Put_Line (Standard_Error, "usage: ruler [length]");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "Output a ruler for describing character positions.");
      GNAT.OS_Lib.OS_Exit (1);
   end Usage;
begin
   if Ada.Command_Line.Argument_Count not in 0..1 then 
      Usage;
   end if;
   
   if Ada.Command_Line.Argument_Count = 1 then
      -- Length of ruler specified on command line.
      begin
         Ruler_Length := Positive'Value (Ada.Command_Line.Argument (1));
      exception
         when Constraint_Error =>
            Put_Line (Standard_Error, 
              -(+"ruler: ""%s"" is an invalid value for ruler length." & Ada.Command_Line.Argument (1)));
            GNAT.OS_Lib.OS_Exit (2);
      end;
   end if;
   
   Number_Of_Parts := Ruler_Length / 10;
   for I in 1..Number_Of_Parts loop
      Put (-(+"%10d" & I));
   end loop;
   New_Line;
   for I in 1..Ruler_Length loop
      Put (-(+"%d" & I mod 10));
   end loop;
   New_Line;
   
   GNAT.OS_Lib.OS_Exit (0);
end Ruler;
