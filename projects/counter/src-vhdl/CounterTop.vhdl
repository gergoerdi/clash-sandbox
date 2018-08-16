library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;
use work.counter_types.all;

entity CounterTop is
  port(CLK_32MHZ : in std_logic;
       BUTTON_LEFT : in std_logic;
       BUTTON_RIGHT : in std_logic;
       SS_ANODES : out counter_types.array_of_std_logic(0 to 3);
       SS_SEGS   : out counter_types.array_of_std_logic(0 to 6);
       SS_DP     : out std_logic;
       LED       : out counter_types.array_of_std_logic(0 to 0));
end;

architecture rtl of CounterTop is
  signal RESET : std_logic := '0';
  signal RESET_CNT : std_logic_vector(2 downto 0) := (others => '1');
begin
  process (CLK_32MHZ, RESET_CNT)
  begin
    if rising_edge(CLK_32MHZ) then
      if not (RESET_CNT = "000") then
        RESET <= '1';
        RESET_CNT <= std_logic_vector(unsigned(RESET_CNT(2 downto 0)) - 1);
      else
        RESET <= '0';
      end if; 
    end if;   
  end process;
  
  inst_Counter : entity work.Counter
  port map (CLK_32MHZ => CLK_32MHZ,
            RESET => RESET,
            BUTTON => BUTTON_LEFT,
            SS_ANODES => SS_ANODES,
            SS_SEGS => SS_SEGS,
            SS_DP => SS_DP,
            LED => LED);
end;

