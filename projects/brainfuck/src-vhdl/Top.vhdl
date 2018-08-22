library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.all;
use work.brainfuck_types.all;

entity Top is
  port(CLK_32MHZ : in std_logic;
       BUTTON    : in std_logic;
       SWITCHES  : in array_of_std_logic(0 to 7);
       JOY_UP    : in std_logic;
       JOY_DOWN  : in std_logic;
       JOY_LEFT  : in std_logic;
       JOY_RIGHT : in std_logic;
       RX        : in std_logic;
       LEDS      : out array_of_std_logic(0 to 7);
       SS_ANODES : out array_of_std_logic(0 to 3);
       SS_SEGS   : out array_of_std_logic(0 to 6);
       SS_DP     : out std_logic;
       TX        : out std_logic);
end;

architecture rtl of Top is
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

  inst_Brainfuck : entity work.Brainfuck
  port map (CLK_32MHZ => CLK_32MHZ,
            RESET => RESET,
            RX => RX,
            BUTTON => JOY_LEFT,
            SWITCHES => SWITCHES,
            TX => TX,
            SS_ANODES => SS_ANODES,
            SS_SEGS => SS_SEGS,
            SS_DP => SS_DP,
            LED => LEDS(0 to 1));

  LEDS(2 to 7) <= (others => '0');
end;
