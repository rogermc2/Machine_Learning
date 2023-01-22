
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Gnat.Regpat; use Gnat.Regpat;

with ML_Types;
with Regexep;

procedure Regex_Tester is

   --  -------------------------------------------------------------------------

   use ML_Types.String_Package;
   use Regexep.Matches_Package;
   --  [a-zA-Z]+ selects a sequence [] of one or more (+)
   --  alphabetic characters (a-zA-Z)
   --  () combines [a-zA-Z]+ into a group
--     Word_Pattern  : constant String := "\\([0-9]{1,3}|u[0-9a-f]{4}|.)";
   Word_Pattern  : constant String := "(\d+)x(\d+)";

   Matcher       : constant Pattern_Matcher := Compile (Word_Pattern);
--     Str           : constant String := "\123 \test \u23af \. rt";
   Str           : constant String := "1920x1600";
--     Current_First : Positive := Str'First;
   First         : Positive;
   Last          : Positive;
   Words         : ML_Types.String_List;
   Match_Curs    : Regexep.Matches_Package.Cursor;
   Matches       : Regexep.Matches_List;
   Groups        : Regexep.Matches_List;
   aMatch        : GNAT.Regpat.Match_Location;
   aGroup        : GNAT.Regpat.Match_Location;
   Found         : Boolean := True;
begin
    New_Line;
    Put_Line ("Regex_Tester, parenthese count:" &
                Integer'Image (Paren_Count (Matcher)));
    New_Line;

   --  Find all the words in Str
   Put_Line ("Find_Match results for matching "  & Str & " with " &
   Word_Pattern);
      Matches := Regexep.Find_Match (Matcher, Str, First, Last, Found);
      if Found then
         Words.Append (To_Unbounded_String (Str (First .. Last)));
         Put_Line ("Match:  <" & Str (First .. Last) & ">");
            New_Line;
            Put_Line ("Match List:");
            Match_Curs := Matches.First;
            while Has_Element (Match_Curs) loop
                aMatch := Element (Match_Curs);
                Put_Line (Str (aMatch.First .. aMatch.Last));
                Next (Match_Curs);
            end loop;

            New_Line;
            Put_Line ("Groups:");
            Groups := Regexep.Get_Groups (Matches);
            Match_Curs := Groups.First;
            while Has_Element (Match_Curs) loop
                aGroup := Element (Match_Curs);
--                  Put_Line (Integer'Image (aGroup.First) & ", " & Integer'Image (aGroup.Last));
                Put_Line (Str (aGroup.First .. aGroup.Last));
                Next (Match_Curs);
            end loop;
      else
         Put_Line ("No match");
      end if;

end Regex_Tester;
