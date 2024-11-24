with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_LIb;
with GNAT.Strings; use GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Formatted_String; use GNAT.Formatted_String;
procedure Ruler is 
   
   Config : Command_Line_Configuration;
   Extra : Boolean := False;
   Ruler_Length : Positive := 80;
   Number_Of_Parts : Positive;
   End_Of_Arguments : Boolean;
   Number_Of_Arguments : Natural := 0;
   
   procedure Callback (Switch, Param, Section : String) is 
   begin
      if Switch = "-e" then
         Extra := True;
      end if ;
   end Callback;
   
   procedure Extra_Line is 
   begin
      if Extra then 
         for I in 1..Ruler_Length loop 
            Put (if I mod 10 = 0 then '+' else '-');
         end loop;
         New_Line;
      end if;
   end Extra_Line;

begin
   Define_Switch (Config, "-e", Help => "Output extra header and trailer lines.");
   Getopt (Config, Callback'Unrestricted_access);
   Set_Usage (Config, "[-e] [ruler_length]");
   
   loop 
      declare 
         Argument : String := Get_Argument (End_Of_Arguments => End_Of_Arguments);
      begin
         exit when End_Of_Arguments;
         Number_Of_Arguments := @ + 1;
         if Number_Of_Arguments > 1 then 
            Put_Line (Standard_Error, "ruler: only one argument is allowed.");
            GNAT.OS_Lib.OS_Exit (2);
         end if;
         Ruler_Length := Positive'Value (Argument);
      exception
         when Constraint_Error =>
            Put_Line (Standard_Error, 
              -(+"ruler: ""%s"" is an invalid value for ruler length." & Argument));
            GNAT.OS_Lib.OS_Exit (3);
      end;
   end loop;
   
   Extra_Line;
   Number_Of_Parts := Ruler_Length / 10;
   for I in 1..Number_Of_Parts loop
      Put (-(+"%10d" & I));
   end loop;
   New_Line;
   for I in 1..Ruler_Length loop
      Put (-(+"%d" & I mod 10));
   end loop;
   New_Line;
   Extra_Line;
   
   GNAT.OS_Lib.OS_Exit (0);
end Ruler;
