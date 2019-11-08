import React from 'react';
import styled from 'styled-components';

import Typography from '@material-ui/core/Typography';

export default function() {
  const offerChainInfo = {
    chainHost: 'test',
    chainId: 'test',
    token: {
      symbol: 'ABT',
      decimal: 18,
    },
  };

  const demandChainInfo = {
    chainHost: 'test',
    chainId: 'test',
    token: {
      symbol: 'ABT',
      decimal: 18,
    },
  };

  const appInfo = {
    name: 'EventTown',
    description:
      'You’re going to purchase the ticket(s) of “ArcBlock Devc0n 2019 - Hackathon" from Eventown.',
  };

  const swapInfo = {
    user_did: 'z1k2aXFUpRBAqJo7HuwyMn49WNgcpc8zbUS',
    asset_owner: 'z1k2aXFUpRBAqJo7HuwyMn49WNgcpc8zbUS',
    status: 'not_started',
    offer_assets: ['z1k2aXFUpRBAqJo7HuwyMn49WNgcpc8zbUS', 'z1k2aXFUpRBAqJo7HuwyMn49WNgcpc8zbUS'],
    offer_token: 5,
    offer_chain: 'test',
    offer_locktime: 2800,
    demand_assets: ['z1k2aXFUpRBAqJo7HuwyMn49WNgcpc8zbUS', 'z1k2aXFUpRBAqJo7HuwyMn49WNgcpc8zbUS'],
    demand_token: 10,
    demand_chain: 'test',
    demand_locktime: 5600,
  };

  return (
    <Container>
      <div className="content-header">
        <Typography component="div" className="logo">
          <img src="/images/wallet.png" className="logo__image" alt="" />
          <Typography className="logo__text">ABT Wallet</Typography>
        </Typography>
      </div>
    </Container>
  );
}

const Container = styled.div`
  .logo {
    display: flex;
    justify-content: flex-start;
    align-items: center;

    .logo__image {
      width: 36px;
      height: 36px;
      margin-right: 8px;
      background-color: #fff;
      border-radius: 8px;
    }

    .logo__text {
      font-size: 24px;
      text-transform: uppercase;
      color: white;
      font-weight: 900;
      letter-spacing: 2px;
    }
  }
`;
