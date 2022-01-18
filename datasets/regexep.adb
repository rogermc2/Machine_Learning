
package body Regexep is

    function Find_Match
      (Compiled_Expression : Gnat.Regpat.Pattern_Matcher; Text : String;
       First, Last         : out Positive; Found : out Boolean;
       Group_Index         : Natural := 0) return Matches_List is
        Num_Parens : constant Natural := Paren_Count (Compiled_Expression);
        Groups     : Match_Array (0 .. Num_Parens);
        Matches    : Matches_List;
        Result     : Match_Location;
    begin
        --  Match selects the first substring of Text that matches
        --  the Compiled_Expression
        Match (Compiled_Expression, Text, Groups);
        Found := Groups (0) /= No_Match;

        if Found then
            for index in 0 .. Num_Parens loop
                Matches.Append (Groups (index));
            end loop;
            Result := Groups (Group_Index);
            First := Result.First;
            Last := Result.Last;
        end if;

        return Matches;

    end Find_Match;

    --  -------------------------------------------------------------------------

    function Get_Groups (Matches : Matches_List) return Matches_List is
        use Ada.Containers;
        Groups : Matches_List;
    begin
        if Matches.Length > 1 then
            for index in Matches.First_Index + 1 .. Matches.Last_Index loop
                Groups.Append (Matches.Element (index));
            end loop;
        end if;

        return Groups;

    end Get_Groups;

    --  -------------------------------------------------------------------------

    function Get_Groups (Matches : Match_Strings_List) return Match_Strings_List is
        use Ada.Containers;
        Groups : Match_Strings_List;
    begin
        if Matches.Length > 1 then
            for index in Matches.First_Index + 1 .. Matches.Last_Index loop
                Groups.Append (Matches.Element (index));
            end loop;
        end if;

        return Groups;

    end Get_Groups;

    --  -------------------------------------------------------------------------

end Regexep;
