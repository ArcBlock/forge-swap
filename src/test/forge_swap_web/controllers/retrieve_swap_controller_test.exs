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

  @owner %{
    address: "z1ewYeWM7cLamiB6qy6mDHnzw1U5wEZCoj7",
    pk:
      <<117, 203, 141, 163, 125, 31, 190, 56, 26, 215, 25, 10, 206, 28, 135, 228, 209, 49, 42,
        104, 155, 38, 5, 244, 194, 122, 44, 158, 28, 230, 60, 197>>,
    sk:
      <<180, 193, 254, 213, 9, 13, 214, 69, 24, 194, 14, 175, 95, 22, 54, 203, 76, 42, 104, 69,
        106, 148, 81, 97, 25, 38, 53, 239, 184, 60, 103, 82, 117, 203, 141, 163, 125, 31, 190, 56,
        26, 215, 25, 10, 206, 28, 135, 228, 209, 49, 42, 104, 155, 38, 5, 244, 194, 122, 44, 158,
        28, 230, 60, 197>>
  }

  @tag :integration
  test "Retrieve swap, all good", %{conn: conn} do
    # Create a Swap 
    body = %{
      "userDid" => @user.address,
      "offerToken" => @offer_token,
      "demandToken" => @demand_token
    }

    %{"response" => %{"id" => id}} =
      conn
      |> post(Routes.swap_path(conn, :create), body)
      |> json_response(200)

    swap = Swap.get(id)
    change = Swap.update_changeset(swap, %{status: "both_set_up", offer_swap: @offer_swap})
    apply(Repo, :update!, [change])

    # Step 1, Wallet scans the QR code
    %{"appPk" => pk, "authInfo" => auth_info} =
      conn
      |> get(Routes.retrieve_swap_path(conn, :retrieve, id))
      |> json_response(200)

    auth_body = Util.get_auth_body(auth_info)
    Util.assert_common_auth_info(pk, auth_body)
    assert auth_body["url"] === Routes.retrieve_swap_url(@endpoint, :retrieve_re_user, id)

    assert auth_body["requestedClaims"] == [
             %{
               "type" => "did",
               "didType" => "account",
               "meta" => %{
                 "description" => "Please proof you won DID #{@user.address} before start.",
                 "target" => "#{@user.address}"
               }
             }
           ]

    # Step 2, Wallet returns user did
    %{"response" => %{"swapAddress" => swap_address}} =
      conn
      |> post(auth_body["url"], Util.gen_signed_request(@user, %{}))
      |> json_response(200)

    assert swap_address == @offer_swap
  end
end
