-- Ada Test File for UAST-Grep
-- Tests: packages, procedures, functions, types, control flow

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

-- Package specification
package Test is

   -- Constants
   Max_Items : constant := 100;
   Default_Name : constant String := "UAST-Grep";

   -- Enumeration type
   type Status is (OK, Not_Found, Server_Error);

   -- Range type
   subtype Percentage is Integer range 0 .. 100;

   -- Record type
   type Person is record
      Name      : Unbounded_String;
      Age       : Natural;
      Email     : Unbounded_String;
      Is_Active : Boolean := True;
   end record;

   -- Array type
   type Int_Array is array (Positive range <>) of Integer;
   type Fixed_Array is array (1 .. 10) of Integer;

   -- Access type (pointer)
   type Person_Access is access all Person;

   -- Generic package instantiation
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);

   -- Tagged type (class-like)
   type Processor is tagged record
      Name  : Unbounded_String;
      Count : Natural := 0;
   end record;

   -- Primitive operations
   procedure Initialize (P : in out Processor; Name : String);
   procedure Log (P : Processor; Message : String);
   function  Process (P : in out Processor; Items : Int_Array) return Int_Array;

   -- Derived type (inheritance)
   type Data_Processor is new Processor with record
      Cache_Size : Natural := 0;
   end record;

   -- Overriding operation
   overriding
   function Process (P : in Out Data_Processor; Items : Int_Array) return Int_Array;

   -- Standalone subprograms
   function Calculate_Sum (A, B : Integer) return Integer;
   function Transform (Item : Integer) return Integer;
   procedure Print_Array (Arr : Int_Array);

   -- Generic function
   generic
      type Element_Type is private;
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
   function Generic_Find (Arr : array (Positive range <>) of Element_Type;
                          Item : Element_Type) return Natural;

   -- Exception declaration
   Processing_Error : exception;
   Invalid_Input    : exception;

end Test;

-- Package body
package body Test is

   -- Procedure implementation
   procedure Initialize (P : in Out Processor; Name : String) is
   begin
      P.Name := To_Unbounded_String (Name);
      P.Count := 0;
   end Initialize;

   -- Log procedure
   procedure Log (P : Processor; Message : String) is
   begin
      Put_Line ("[" & To_String (P.Name) & "] " & Message);
   end Log;

   -- Process function for base type
   function Process (P : in Out Processor; Items : Int_Array) return Int_Array is
      Results : Int_Array (Items'Range);
   begin
      -- For loop
      for I in Items'Range loop
         Results (I) := Transform (Items (I));
      end loop;

      P.Count := Items'Length;
      return Results;

   exception
      when others =>
         P.Log ("Error during processing");
         raise Processing_Error;
   end Process;

   -- Overriding process for derived type
   overriding
   function Process (P : in Out Data_Processor; Items : Int_Array) return Int_Array is
      Results : Int_Array (Items'Range);
   begin
      -- While loop
      declare
         I : Positive := Items'First;
      begin
         while I <= Items'Last loop
            Results (I) := Transform (Items (I)) * 2;
            I := I + 1;
         end loop;
      end;

      P.Count := Items'Length;
      P.Cache_Size := Items'Length;
      return Results;
   end Process;

   -- Calculate sum function
   function Calculate_Sum (A, B : Integer) return Integer is
   begin
      return A + B;
   end Calculate_Sum;

   -- Transform function with case statement
   function Transform (Item : Integer) return Integer is
   begin
      -- Case statement
      case Item is
         when Integer'First .. -1 =>
            return Item * -2;
         when 0 =>
            return 0;
         when 1 .. 10 =>
            return Item * 2;
         when others =>
            return Item;
      end case;
   end Transform;

   -- Print array procedure
   procedure Print_Array (Arr : Int_Array) is
   begin
      Put ("Array: [");
      for I in Arr'Range loop
         Put (Arr (I), Width => 0);
         if I /= Arr'Last then
            Put (", ");
         end if;
      end loop;
      Put_Line ("]");
   end Print_Array;

   -- Generic function body
   function Generic_Find (Arr : array (Positive range <>) of Element_Type;
                          Item : Element_Type) return Natural is
   begin
      for I in Arr'Range loop
         if Arr (I) = Item then
            return I;
         end if;
      end loop;
      return 0;  -- Not found
   end Generic_Find;

end Test;

-- Main procedure
procedure Main is
   use Test;

   -- Variables
   P : Processor;
   DP : Data_Processor;
   Data : Int_Array := (1, 2, 3, 4, 5);
   Results : Int_Array (1 .. 5);
   Sum : Integer;
   Person_Rec : Person;

begin
   -- Initialize processors
   Initialize (P, "Main");
   Initialize (Processor (DP), "DataProcessor");

   -- Process data
   Results := Process (P, Data);
   Print_Array (Results);

   -- If statement
   if Results'Length > 0 then
      Sum := 0;
      for Val of Results loop
         Sum := Sum + Val;
      end loop;
      Put ("Sum: "); Put (Sum, Width => 0); New_Line;
   else
      Put_Line ("No results");
   end if;

   -- If-elsif-else
   if Sum > 100 then
      Put_Line ("Large sum");
   elsif Sum > 50 then
      Put_Line ("Medium sum");
   else
      Put_Line ("Small sum");
   end if;

   -- Record usage
   Person_Rec := (Name      => To_Unbounded_String ("Alice"),
                  Age       => 30,
                  Email     => To_Unbounded_String ("alice@example.com"),
                  Is_Active => True);

   -- Exception handling
   begin
      if Person_Rec.Age < 0 then
         raise Invalid_Input with "Age cannot be negative";
      end if;
   exception
      when E : Invalid_Input =>
         Put_Line ("Caught: Invalid input");
      when others =>
         Put_Line ("Unknown error");
   end;

   Log (P, "Processing complete");

end Main;
