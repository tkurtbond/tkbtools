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

   function Error_To_String (Error_Number : in HINSTANCE) return String is
      function To_Integer is new Ada.Unchecked_Conversion (Address, Integer);
      FORMAT_MESSAGE_FROM_SYSTEM : constant := 4096;
      MAX_ERROR                  : constant := 1024;
      Message                    : aliased String (1 .. MAX_ERROR) :=
        (others => ASCII.NUL);
      Message_Ptr                : Address := Message(Message'First)'Address;

      function FormatMessage
        (dwFlags      : Integer                    :=
           FORMAT_MESSAGE_FROM_SYSTEM;
         lpSource     : Interfaces.C.unsigned_long := 0;
         hr           : Integer                    := To_Integer (Error_Number);
         dwLanguageId : Interfaces.C.unsigned_long := 0;
         lpBuffer     : Address                    := Message_Ptr;
         nSize        : Interfaces.C.unsigned_long := MAX_ERROR;
         Arguments    : Interfaces.C.unsigned_long := 0)
        return Interfaces.C.Unsigned_Long;
      pragma Import (StdCall, FormatMessage, "FormatMessageA");
      Len : Interfaces.C.Unsigned_Long;
   begin
      Len := FormatMessage;
      return  Message (Message'First .. Integer (Len));
   end Error_To_String;

   function Windows_Error (Ret: HINSTANCE) return Boolean is
      function To_Integer is new Ada.Unchecked_Conversion (Address, Integer);
      N: Integer := To_Integer (Ret);
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
            Put_Line ("s1: " & Error_To_String (Ret));
         end if;
         Put_Line  ("Ret: " & Fmt (Ret) & " for "
                    & "Arg(" & Fmt (I) & "): " & S);
      end;
   end loop;
end S1;
