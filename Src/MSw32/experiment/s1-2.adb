with System; use System;

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Win32; use Win32;
with Win32.ShellAPI; use Win32.ShellAPI;
with Win32.Windef; use Win32.Windef;
with Win32.Winuser; use Win32.Winuser;
with Win32.Winbase; use Win32.Winbase;

with TKB; use TKB;


procedure S1 is
   package CL renames Ada.Command_Line;
   Debugging: constant Boolean := False;

   function Address_To_Integer is 
      new Ada.Unchecked_Conversion (Address, Integer);

   function Error_To_String (Error_Number : in HINSTANCE) return String is
      FORMAT_MESSAGE_FROM_SYSTEM : constant := 4096;
      MAX_ERROR                  : constant := 1024;
      Message                    : aliased String (1 .. MAX_ERROR) :=
        (others => ASCII.NUL);
      Message_Ptr                : Address := Message(Message'First)'Address;

      function FormatMessage
        (dwFlags      : Integer := FORMAT_MESSAGE_FROM_SYSTEM;
         lpSource     : Integer := 0;
         hr           : Integer := Address_To_Integer (Error_Number);
         dwLanguageId : Integer := 0;
         lpBuffer     : Address := Message_Ptr;
         nSize        : Integer := MAX_ERROR;
         Arguments    : Integer := 0)
        return Integer;
      pragma Import (StdCall, FormatMessage, "FormatMessageA");
      Len : Integer;
   begin
      Len := FormatMessage;
      if Message(Len) = ASCII.LF then 
	 Len := Len - 1;
      end if;
      if Message (Len) = ASCII.CR then 
	 Len := Len - 1;
      end if;
      return  Message (Message'First .. Integer (Len));
   end Error_To_String;

   function Windows_Error (Ret: HINSTANCE) return Boolean is
      N: Integer := Address_To_Integer (Ret);
   begin
      return N <= 32;
   end Windows_Error;

   Ret: HINSTANCE;
begin
   for I in 1 .. CL.Argument_Count loop
      declare
         S: String := CL.Argument (I);
         NS: String := S & ASCII.NUL;
         PS: PCSTR := Addr (NS);
      begin
         Ret := ShellExecute (Null_Address, null, PS, null, null,
                              SW_SHOWNORMAL);
         if Windows_Error (Ret) then
	    declare S : String := Error_To_String (Ret);
	    begin
	       Put_Line (Standard_Error, "s1: " & S);
	    end;	    
         end if;
	 if debugging then 
	    Put_Line  ("Ret: " & Fmt (Ret) & " for "
		       & "Arg(" & Fmt (I) & "): " & S);
	 end if;
      end;
   end loop;
end S1;
