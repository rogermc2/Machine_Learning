
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Gnat.Regpat; use Gnat.Regpat;

with ML_Types;
with Regexep;

procedure Regex_Tester is

   --  -------------------------------------------------------------------------

   use ML_Types.String_Package;
   --  [a-zA-Z]+ selects a sequence [] of one or more (+)
   --  alphabetic characters (a-zA-Z)
   --  () combines [a-zA-Z]+ into a group
--     Word_Pattern  : constant String := "\\([0-9]{1,3}|u[0-9a-f]{4}|.)";
   Word_Pattern  : constant String := "ab\d";

   Matcher       : constant Pattern_Matcher := Compile (Word_Pattern);
--     Str           : constant String := "\123 \test \u23af \. rt";
   Str           : constant String := "ab123e5f6g";
   Current_First : Positive := Str'First;
   First         : Positive;
   Last          : Positive;
   Words         : ML_Types.String_List;
   Curs          : Cursor;
   Found         : Boolean := True;
begin
    New_Line;
    Put_Line ("Regex_Tester, parenthese count:" &
                Integer'Image (Paren_Count (Matcher)));
    New_Line;

   --  Find all the words in Str
   Put_Line ("Find_Match results for matching "  & Str & " with " &
   Word_Pattern);
   while Found loop
      Regexep.Find_Match (Matcher, Str (Current_First .. Str'Last),
                        First, Last, Found);
      if Found then
         Words.Append (To_Unbounded_String (Str (First .. Last)));
         Put_Line ("<" & Str (First .. Last) & ">");
      else
         Put_Line ("No match");
      end if;
      Current_First := Last + 1;
   end loop;

   New_Line;
   Put_Line ("String_List:");
   Curs := Words.First;
   while Has_Element (Curs) loop
      Put_Line ("<<" & Element (Curs) & ">>");
      Next (Curs);
   end loop;

   New_Line;
   Put_Line ("Groups:");
   Curs := Words.First;
   while Has_Element (Curs) loop
      Put_Line (Regexep.Get_Groups (To_String (Element (Curs))));
      Next (Curs);
   end loop;

end Regex_Tester;
