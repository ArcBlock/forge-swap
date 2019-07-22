defmodule ForgeSwapWeb.SwapControllerTest do
  use ForgeSwapWeb.ConnCase
  import Mock

  alias ForgeSwapWebTest.Util
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Schema.Swap

  @user1 %{
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

  @user2 %{
    address: "z1d8ZCDR3GrvWW233HLJBMdJn39hVEy3TCr",
    pk:
      <<156, 195, 43, 41, 100, 46, 50, 179, 213, 94, 216, 136, 240, 190, 42, 201, 23, 251, 242,
        194, 66, 61, 181, 30, 207, 100, 153, 191, 181, 165, 236, 5>>,
    sk:
      <<15, 114, 113, 212, 252, 179, 209, 219, 11, 217, 187, 88, 125, 105, 91, 16, 146, 235, 54,
        49, 96, 238, 113, 192, 150, 171, 243, 221, 245, 161, 3, 125, 156, 195, 43, 41, 100, 46,
        50, 179, 213, 94, 216, 136, 240, 190, 42, 201, 23, 251, 242, 194, 66, 61, 181, 30, 207,
        100, 153, 191, 181, 165, 236, 5>>
  }

  @default_owner "z1ewYeWM7cLamiB6qy6mDHnzw1U5wEZCoj7"

  test "Create swap, all good", %{conn: conn} do
    body = %{
      "userDid" => "zabc",
      "offerAssets" => ["z123", "z456"],
      "demandToken" => 1_000_000_000_000
    }

    %{"response" => %{"id" => id}} =
      conn
      |> post(Routes.swap_path(conn, :create), body)
      |> json_response(200)

    assert is_number(id)
  end

  test "Start swap, all good", %{conn: conn} do
    body = %{
      "userDid" => @user1.address,
      "offerAssets" => ["z123", "z456"],
      "demandToken" => 1_000_000_000_000
    }

    %{"response" => %{"id" => id}} =
      conn
      |> post(Routes.swap_path(conn, :create), body)
      |> json_response(200)

    %{"appPk" => _, "authInfo" => auth_info} =
      conn
      |> post(Routes.swap_path(conn, :start, id), Util.gen_signed_request(@user1, %{}))
      |> json_response(200)

    auth_body = Util.get_auth_body(auth_info)

    assert auth_body["appInfo"] == %{
             "description" =>
               "A blockchain-based ticket-selling application that allows event hosts and participants to have full control of every ticket.",
             "name" => "Event Chain"
           }

    assert auth_body["chanInfo"] == %{"host" => "http://127.0.0.1:8410/api"}
    assert auth_body["url"] == "http://localhost:8807/api/swap/#{id}/submit"
    assert auth_body["iss"] == "did:abt:z1ewYeWM7cLamiB6qy6mDHnzw1U5wEZCoj7"
    assert not is_nil(auth_body["exp"])
    assert not is_nil(auth_body["iat"])
    assert not is_nil(auth_body["nbf"])

    assert auth_body["requestedClaims"] == [
             %{
               "didType" => "swap",
               "meta" => %{
                 "demandAssets" => [],
                 "demandChain" => "http://127.0.0.1:8310/api",
                 "demandLocktime" => 48,
                 "demandToken" => 1_000_000_000_000,
                 "description" => "Please set up an atomic swap on the ABT asset chain.",
                 "offerAssets" => ["z123", "z456"],
                 "offerToken" => 0
               },
               "type" => "did"
             }
           ]
  end

  test "Submit swap, all good", %{conn: conn} do
    demand_token = 1_000_000_000_000
    mock_hash = "192A1C3ED1C9D1C7BF8DB54D0E72245E479F99607FB03A43DC3076E96A2359EA"
    mock_address = "z2UHtVbPeX1yJcmsaCBYK1DenXKUmyrXSo4aJ"

    body = %{
      "userDid" => @user1.address,
      "offerAssets" => ["z123", "z456"],
      "demandToken" => demand_token
    }

    %{"response" => %{"id" => id}} =
      conn
      |> post(Routes.swap_path(conn, :create), body)
      |> json_response(200)

    actual_swap_state = %{
      "address" => mock_address,
      "sender" => @user1.address,
      "receiver" => @default_owner,
      "value" => "#{demand_token}",
      "assets" => [],
      "hashlock" => :crypto.strong_rand_bytes(32) |> Base.encode16()
    }

    with_mocks([
      {
        ChainUtil,
        [:passthrough],
        [
          get_swap_sate: fn _, _ -> actual_swap_state end,
          time_to_locktime: fn _, _ -> 10000 end,
          send_tx: fn _, _ -> mock_hash end
        ]
      }
    ]) do
      body =
        Util.gen_signed_request(@user1, %{
          "requestedClaims" => [%{"type" => "did", "didType" => "swap", "did" => mock_address}]
        })

      %{"response" => %{"hash" => ^mock_hash}} =
        conn
        |> post(Routes.swap_path(conn, :submit, id), body)
        |> json_response(200)

      swap = Swap.get(id)
      assert swap.demand_swap === mock_address
      assert swap.offer_swap === mock_address
      assert swap.status === "both_set_up"
    end
  end
end
