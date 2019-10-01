defmodule ForgeSwap.Utils.Util do
  require Logger

  alias ForgeSwapWeb.Router.Helpers
  alias ForgeSwapWeb.Endpoint

  def str_to_bin("0x" <> str), do: Multibase.decode!("f" <> str)

  def str_to_bin(str) do
    case Base.decode16(str, case: :mixed) do
      {:ok, bin} ->
        bin

      _ ->
        case Multibase.decode(str) do
          {:ok, bin} -> bin
          _ -> Base.url_decode64!(str, padding: false)
        end
    end
  end

  # Generates the QR code by scanning which a user can start the swap process.
  def gen_qr_code("not_started", swap_id),
    do: Endpoint |> Helpers.payment_url(:start, swap_id) |> Hyjal.Util.get_qr_code()

  # Generates the QR code by scanning which a user can continue to retrieve the swap set up by application.
  def gen_qr_code("both_set_up", swap_id),
    do: Endpoint |> Helpers.retrieve_swap_url(:start, swap_id) |> Hyjal.Util.get_qr_code()

  def gen_qr_code(_, _), do: ""

  def find_claim(claims, claim_module, func) do
    Enum.find(claims, fn claim -> claim.__struct__ == claim_module and func.(claim) end)
  end
end
