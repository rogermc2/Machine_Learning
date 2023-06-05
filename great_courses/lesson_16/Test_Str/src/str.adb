--  From https://stackoverflow.com/questions/59952682/how-can-i-return-a-string-allocated-in-c-to-ada-and-free-it-in-ada

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;

procedure Str is

   function Get_Str return chars_ptr with
     Import => True,
     Convention => C,
     External_Name => "get_str";

   procedure Get_Str (Str : in out chars_ptr) with
     Import => True,
     Convention => C,
     External_Name => "get_str2";

   Str  : chars_ptr := Get_Str;
   Str2 : chars_ptr := Null_Ptr;
begin
   Get_Str (Str2);

   Put ("==> " & Value (Str));
   Put ("==> " & Value (Str2));

   Free (Str);
   Free (Str2);
end Str;
