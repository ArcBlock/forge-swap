defmodule ForgeSwapWeb.RetrieveSwapControllerTest do
  use ForgeSwapWeb.ConnCase

  alias ForgeSwapWebTest.Util
  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Repo

  @offer_swap "z123"
  @offer_token 111_111_111_111_111
  @demand_token 222_222_222_222_222
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

  @tag :integration
  test "Retrieve swap, all good", %{conn: conn} do
    # Create a Swap 

    {:ok, %{id: id}} =
      %{
        "userDid" => @user.address,
        "offerToken" => @offer_token,
        "demandToken" => @demand_token
      }
      |> Util.create_swap()
      |> Repo.insert()

    swap = Swap.get(id)
    change = Swap.update_changeset(swap, %{status: "both_set_up", offer_swap: @offer_swap})
    apply(Repo, :update!, [change])

    # Step 1, Wallet scans the QR code
    %{"appPk" => pk, "authInfo" => auth_info} =
      conn
      |> get(Routes.retrieve_swap_path(conn, :start, id))
      |> json_response(200)

    auth_body = Util.get_auth_body(auth_info)
    Util.assert_common_auth_info(pk, auth_body)
    assert auth_body["url"] === Routes.retrieve_swap_path(@endpoint, :auth_principal, id)

    assert auth_body["requestedClaims"] == [
             %{
               "type" => "authPrincipal",
               "description" => "Please set the authentication principal to the specified DID.",
               "target" => "#{@user.address}",
               "meta" => nil
             }
           ]

    # Step 2, Wallet returns user did
    %{"appPk" => pk, "authInfo" => auth_info} =
      conn
      |> post(auth_body["url"], Util.gen_signed_request(@user, %{}))
      |> json_response(200)

    auth_body = Util.get_auth_body(auth_info)
    Util.assert_common_auth_info(pk, auth_body)
    %{"response" => %{"swapAddress" => swap_address}} = auth_body
    assert swap_address == @offer_swap
  end
end
