import { createLogger as _createLogger, format, transports } from "winston";

const logCache = new Map();

const levelToLevel = {
  Error: "error",
  Warning: "warning",
  Info: "info",
  Debug: "debug",
};

export const mkLogger = (levelType) => (namespace) => () => {
  const level = levelType.constructor.name;
  const logger = _createLogger({
    level: levelToLevel[level] || "info",
    levels: {
      error: 0,
      warning: 1,
      info: 2,
      debug: 3,
    },
    format: format.combine(
      format.label({ label: namespace }),
      format.timestamp({ format: "mm:ss" }),
      format.colorize(),
      format.printf((info) => `${info.timestamp} ${info.label} ${info.level}: ${info.message}`),
    ),
    transports: [new transports.Console()],
  });

  if (logCache.has(namespace)) {
    return logCache.get(namespace);
  }

  logCache.set(namespace, logger);

  return {
    error: (message) => () => logger.error(message),
    warning: (message) => () => logger.warning(message),
    info: (message) => () => logger.info(message),
    debug: (message) => () => logger.debug(message),
  };
};
