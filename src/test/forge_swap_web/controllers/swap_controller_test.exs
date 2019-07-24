defmodule ForgeSwapWeb.SwapControllerTest do
  use ForgeSwapWeb.ConnCase
  alias ForgeSwap.Schema.Swap

  @user %{
    address: "z1SWvGbnCiJf5MpWnV8qoYQWrYnK2BdtpaQ",
    pk:
      <<248, 202, 174, 37, 243, 223, 249, 206, 99, 222, 173, 12, 97, 245, 149, 210, 123, 29, 16,
        101, 60, 167, 206, 217, 141, 96, 75, 48, 20, 159, 173, 66>>,
    sk:
      <<130, 122, 74, 55, 242, 33, 15, 235, 96, 163, 197, 239, 122, 63, 172, 44, 67, 33, 94, 158,
        32, 98, 206, 163, 30, 151, 42, 71, 169, 214, 132, 178, 248, 202, 174, 37, 243, 223, 249,
        206, 99, 222, 173, 12, 97, 245, 149, 210, 123, 29, 16, 101, 60, 167, 206, 217, 141, 96,
        75, 48, 20, 159, 173, 66>>
  }

  test "Create swap, all good", %{conn: conn} do
    body = %{
      "userDid" => @user.address,
      "offerAssets" => ["z123", "z456"],
      "demandToken" => 1_000_000_000_000
    }

    %{"response" => %{"id" => id}} =
      conn
      |> post(Routes.swap_path(conn, :create), body)
      |> json_response(200)

    assert is_number(id)
    swap = Swap.get(id)

    assert swap.user_did == @user.address
    assert swap.asset_owner == "default"
    assert swap.status == "not_started"
    assert swap.offer_assets == ["z123", "z456"]
    assert swap.offer_token == Decimal.new(0)
    assert swap.offer_chain == "application"
    assert swap.offer_locktime == 24
    assert swap.demand_assets == []
    assert swap.demand_token == Decimal.new(1_000_000_000_000)
    assert swap.demand_chain == "asset"
    assert swap.demand_locktime == 48
  end
end
