
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Gnat.Regpat; use Gnat.Regpat;

with ML_Types;
with Regex;

procedure Regex_Tester is

   --  -------------------------------------------------------------------------

   use ML_Types.String_Package;
   --  [a-zA-Z]+ selects a sequence [] of one or more (+)
   --  alphabetic characters (a-zA-Z)
   --  () combines [a-zA-Z]+ into a group
   Word_Pattern  : constant String := "\\([0-9]{1,3}|u[0-9a-f]{4}|.)";
   Matcher       : constant Pattern_Matcher := Compile (Word_Pattern);
   Str           : String := "\123 \test \u23af \. rt";
   Current_First : Positive := Str'First;
   First         : Positive;
   Last          : Positive;
   Words         : ML_Types.String_List;
   Curs          : Cursor;
   Found         : Boolean := True;
begin
   --  first, find all the words in Str
   while Found loop
      Regex.Find_Match (Matcher, Str (Current_First .. Str'Last),
                        First, Last, Found);
      if Found then
         Words.Append (To_Unbounded_String (Str (First .. Last)));
         Put_Line ("<" & Str (First .. Last) & ">");
      else
         Put_Line ("No match");
      end if;
      Current_First := Last + 1;
   end loop;

--     second, replace "PATTERN" in Str by "pattern"
       Regex.Find_Match (Compile ("(PATTERN)"), Str, First, Last, Found);
--         pragma Unreferenced (Found);

   if Found then
       Str :=
         Str (Str'First .. First - 1) & "pattern" & Str (Last + 1 .. Str'Last);
       Put_Line (Str);

       New_Line;
       Curs := Words.First;
       while Has_Element (Curs) loop
           Put_Line ("<<" & Element (Curs) & ">>");
           Next (Curs);
      end loop;
   end if;

end Regex_Tester;
