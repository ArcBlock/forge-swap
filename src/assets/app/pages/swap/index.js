import React from 'react';
import PropTypes from 'prop-types';
import axios from 'axios';
import QRCode from 'qrcode.react';
import styled from 'styled-components';
import { withRouter } from 'react-router-dom';
import { fromUnitToToken } from '@arcblock/forge-util';

import useAsync from 'react-use/lib/useAsync';
import useInterval from '@arcblock/react-hooks/lib/useInterval';

import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import CircularProgress from '@material-ui/core/CircularProgress';

import WalletDownload from '@arcblock/ux/lib/Wallet/Download';
import Tag from '@arcblock/ux/lib/Tag';
import DidAddress from '@arcblock/did-react/lib/Address';

const getExplorerUrl = (chainHost, did, type) =>
  chainHost.replace('/api', `/node/explorer/${type}/${did}`);

const fetchConfig = async () => {
  const { data } = await axios.get('/api/config');
  const {
    appInfo,
    serviceInfo,
    offerChainInfo,
    demandChainInfo,
    offerChainToken,
    demandChainToken,
  } = data;
  return {
    appInfo,
    apiBaseUrl: `${serviceInfo.schema}://${serviceInfo.host}:${serviceInfo.port}/api`,
    offerChain: {
      id: offerChainInfo.id,
      host: offerChainInfo.host,
      token: offerChainToken,
    },
    demandChain: {
      id: demandChainInfo.id,
      host: demandChainInfo.host,
      token: demandChainToken,
    },
  };
};

const encodeAuthUrl = url =>
  `https://abtwallet.io/i/?action=requestAuth&url=${encodeURIComponent(url)}`;

const getQRCodeUrl = (swap, apiBaseUrl) => {
  if (swap.status === 'not_started') {
    return encodeAuthUrl(`${apiBaseUrl}/payment/${swap.id}`);
  }
  if (swap.status === 'both_set_up') {
    return encodeAuthUrl(`${apiBaseUrl}/retrievepayment/${swap.id}`);
  }

  return '';
};

function SwapDetail({ match }) {
  const swapId = match.params.id;
  const config = useAsync(fetchConfig);
  const swap = useAsync(async () => {
    try {
      const { data } = await axios.get(`/api/swap/${swapId}`);
      data.id = swapId;
      return data;
    } catch (err) {
      throw new Error(`Swap info for ${swapId} can not be fetched`);
    }
  });

  console.log(config);

  return (
    <Container>
      <div className="header">
        <Typography component="div" className="logo">
          <img src="/images/wallet.png" className="logo__image" alt="" />
          <Typography className="logo__text">ABT Wallet</Typography>
        </Typography>
      </div>
      {(config.loading || !config.value || swap.loading || !swap.value) && (
        <div className="loading">
          <CircularProgress />
        </div>
      )}
      {config.value && swap.value && (
        <React.Fragment>
          <Typography component="h2" className="title">
            Make Payment to {config.value.appInfo.name}
          </Typography>
          <div className="section section-order">
            <Typography component="div" className="section__title">
              Your Order
            </Typography>
            <div className="section__body">
              <div className="info-row">
                <div className="info-row__key">Application</div>
                <div className="info-row__value">{config.value.appInfo.name}</div>
              </div>
              <div className="info-row">
                <div className="info-row__key">Description</div>
                <div className="info-row__value">{config.value.appInfo.description}</div>
              </div>
              <div className="info-row">
                <div className="info-row__key">You will get</div>
                <div className="info-row__value">
                  <div className="info-rows">
                    {swap.value.offer_token > 0 && (
                      <div className="info-row info-row--h">
                        <div className="info-row__key">Token</div>
                        <div className="info-row__value">
                          <Typography component="strong" className="amount">
                            {fromUnitToToken(
                              swap.value.offer_token,
                              config.value.offerChain.token.decimal
                            )}
                          </Typography>{' '}
                          {config.value.offerChain.token.symbol}
                        </div>
                      </div>
                    )}
                    {swap.value.offer_assets.length > 0 &&
                      swap.value.offer_assets.map(x => (
                        <div className="info-row info-row--h" key={x}>
                          <div className="info-row__key">Asset</div>
                          <div className="info-row__value">
                            <DidAddress component="p" copyable={false}>
                              <a
                                target="_blank"
                                href={getExplorerUrl(config.value.offerChain.host, x, 'assets')}>
                                {x}
                              </a>
                            </DidAddress>
                          </div>
                        </div>
                      ))}
                  </div>
                </div>
              </div>
              <div className="info-row">
                <div className="info-row__key">Seller</div>
                <div className="info-row__value">
                  <DidAddress component="p" copyable={false}>
                    <a
                      target="_blank"
                      href={getExplorerUrl(
                        config.value.offerChain.host,
                        config.value.appInfo.did,
                        'accounts'
                      )}>
                      {config.value.appInfo.did}
                    </a>
                  </DidAddress>
                </div>
              </div>
              <div className="info-row">
                <div className="info-row__key">You will pay</div>
                <div className="info-row__value">
                  <div className="info-rows">
                    {swap.value.demand_token > 0 && (
                      <div className="info-row info-row--h">
                        <div className="info-row__key">Token</div>
                        <div className="info-row__value">
                          <Typography component="strong" className="amount">
                            {fromUnitToToken(
                              swap.value.demand_token,
                              config.value.demandChain.token.decimal
                            )}
                          </Typography>{' '}
                          {config.value.demandChain.token.symbol}
                        </div>
                      </div>
                    )}
                    {swap.value.demand_assets.length > 0 &&
                      swap.value.demand_assets.map(x => (
                        <div className="info-row info-row--h" key={x}>
                          <div className="info-row__key">Asset</div>
                          <div className="info-row__value">
                            <DidAddress component="p" copyable={false}>
                              <a
                                target="_blank"
                                href={getExplorerUrl(config.value.demandChain.host, x, 'assets')}>
                                {x}
                              </a>
                            </DidAddress>
                          </div>
                        </div>
                      ))}
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div className="section section-order">
            <Typography component="div" className="section__title">
              Make Payment
            </Typography>
            <div className="section__body">
              <Grid container alignItems="center" spacing={4}>
                <Grid item xs={12} sm={7} className="scan">
                  <Typography component="div" className="scan__title">
                    Scan to Check Out
                  </Typography>
                  <Typography component="div" className="scan__tip">
                    Scan the QR code to make payment on your ABT Wallet app. If you leave the app
                    without finishing the payment, you may come back, re-scan it to active the
                    transaction.
                  </Typography>
                </Grid>
                <Grid item xs={12} sm={5} className="qrcode-wrapper">
                  {!!getQRCodeUrl(swap.value, config.value.apiBaseUrl) && (
                    <div className="qrcode-container">
                      <QRCode
                        size={180}
                        renderAs="svg"
                        level="M"
                        value={getQRCodeUrl(swap.value, config.value.apiBaseUrl)}
                      />
                    </div>
                  )}
                  {!getQRCodeUrl(swap.value, config.value.apiBaseUrl) && (
                    <Tag type="info">{swap.status}</Tag>
                  )}
                </Grid>
              </Grid>
            </div>
          </div>
          <div className="section section-order">
            <Typography component="div" className="section__title">
              Do not have ABT Wallet?
            </Typography>
            <div className="section__body">
              <WalletDownload title="" />
            </div>
          </div>
        </React.Fragment>
      )}
    </Container>
  );
}

SwapDetail.propTypes = {
  match: PropTypes.object.isRequired,
  location: PropTypes.object.isRequired,
  history: PropTypes.object.isRequired,
};

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

  .title {
    margin-top: 96px;
    font-size: 24px;
    font-weight: 500;
  }

  .loading {
    margin-top: 96px;
  }

  .section {
    margin-top: 24px;

    .section__title {
      background-color: #f0f5f5;
      padding: 4px;
      font-weight: 500;
    }

    .section__body {
      padding: 24px 24px 0;
      @media (max-width: 768px) {
        padding: 24px 0;
      }

      .qrcode-container {
        background: #ffffff;
        padding: 16px;
        border-radius: 8px;
        box-shadow: 0 0 8px rgba(0, 0, 0, 0.1);
        width: 212px;
      }

      .info-row {
        display: flex;
        flex-direction: column;
        line-height: 1.6;
        margin-bottom: 16px;

        .info-row__key {
          color: #4a4a4a;
          font-size: 12px;
        }

        .info-row__value {
          color: #222222;
          font-size: 14px;
          font-weight: 500;
        }

        .amount {
          font-size: 20px;
          font-weight: 900;
        }
      }

      .info-row--h {
        flex-direction: row;
        align-items: center;
        margin-bottom: 8px;

        .info-row__key {
          width: 60px;
        }
      }

      .scan__title {
        font-size: 16px;
        margin-bottom: 16px;
      }

      .scan__tip {
        font-size: 14px;
      }

      .download-store-list {
        justify-content: flex-start;
        width: 100%;
      }
    }
  }
`;

export default withRouter(SwapDetail);
