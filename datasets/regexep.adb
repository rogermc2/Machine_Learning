
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Regexep is
   use GNAT.Regpat;

   package Matches_Package is new Ada.Containers.Vectors
     (Natural, Match_Location);
   subtype Matches_List is Matches_Package.Vector;

   function All_Matches
     (Compiled_Expression : Gnat.Regpat.Pattern_Matcher; Text : String;
      Found               : out Boolean) return Matches_List is
      Num_Parens : constant Natural := Paren_Count (Compiled_Expression);
      Matches    : Match_Array (0 .. Num_Parens);
      Result     : Matches_List;
   begin
      --  Match selects the first substring of Text that matches
      --  the Compiled_Expression
      Match (Compiled_Expression, Text, Matches);
      Found := Matches (0) /= No_Match;

      if Found then
         for index in 0 .. Num_Parens loop
            Result.Append (Matches (index));
         end loop;
      end if;

      return Result;

   end All_Matches;

   --  -------------------------------------------------------------------------

   procedure Find_Match
     (Compiled_Expression : Gnat.Regpat.Pattern_Matcher; Text : String;
      First, Last         : out Positive; Found : out Boolean;
      Group_Index         : Natural := 0) is
      Matches : constant Matches_List
        := All_Matches (Compiled_Expression, Text, Found);
      Result  : Match_Location;
   begin
      if Found then
         Result := Matches.Element (Group_Index);
         First := Result.First;
         Last := Result.Last;
      end if;

   end Find_Match;

   --  -------------------------------------------------------------------------

   function Get_Groups (Regex : String) return String is
      UB_Regex : constant Unbounded_String := To_Unbounded_String (Regex);
      Pattern  : constant String := "c";
      Matcher  : constant Pattern_Matcher := Compile (Pattern);
      Found    : Boolean;
      Matches  : constant Matches_List := All_Matches (Matcher, Regex, Found);
      aMatch   : Match_Location;
      Groups   : Unbounded_String := To_Unbounded_String ("");
   begin
      if Found then
         for index in Matches.First_Index .. Matches.Last_Index loop
            aMatch := Matches.Element (index);
            if Slice (UB_Regex, aMatch.First, aMatch.First) = "(" then
               Groups := Groups & Slice (UB_Regex, aMatch.First, aMatch.Last);
            end if;
            Put_Line (Groups);
         end loop;
      end if;

      return To_String (Groups);

   end Get_Groups;

   --  -------------------------------------------------------------------------

end Regexep;
