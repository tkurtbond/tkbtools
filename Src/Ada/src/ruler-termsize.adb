with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

separate (Ruler)
procedure Termsize (Rows : out Natural; Columns : out Natural) is

   type termsize is record
      Rows    : Interfaces.C.int;
      Columns : Interfaces.C.int;
   end record;
   pragma Convention (C, termsize);

   function  Get_Termsize (T : in out termsize) return Interfaces.C.int;
   pragma Import (C, Get_Termsize, "get_termsize");

   function Strerror (error_number : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, strerror);

   T : termsize := (Rows => 0, Columns => 0);
   Result : Interfaces.C.int := Get_Termsize (t);
begin
   if Result /= 0 then          -- There was an error.
      Put_Line ("An error occured! errno:" & Result'Image & ".  Error message: " & Value (Strerror (Result)) & ".");
      Rows := 0;
      Columns := 0;
   else
      Rows := Natural (T.Rows);
      Columns := Natural (T.Columns);
   end if;
end Termsize;
